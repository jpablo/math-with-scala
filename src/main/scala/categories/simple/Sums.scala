package categories.simple

import discipline1.*
import categories.simple.{_, given}

object Ops {
  type Map[Tup <: Tuple, F[_ <: Tuple]] <: Tuple = Tup match
    case EmptyTuple => EmptyTuple
    case h *: t     => F[h] *: Map[t, F]

  type FoldLeft[F[_, _], Acc, T <: Tuple] = T match
    case EmptyTuple => Acc
    case h *: t     => FoldLeft[F, F[Acc, h], t]

  type Union[T <: Tuple] =
    FoldLeft[[Acc, A] =>> Acc | A, Nothing, T]

  // type Union0[T <: Tuple, Acc] =
  //   T match {
  //     case EmptyTuple => Acc
  //     case h *: t => Union[t, h | Acc]
  //   }
}


trait Family2[A, B, X, ~>[_, _]] {
  // val t: (A => (A, 1), B => (B, 2))
  type Arrows = Tuple.Map[(A, B), [Xi] =>> Xi ~> X]
  val t: Arrows
  val _1: A ~> X
  val _2: B ~> X
}

trait Sum2[X1, X2, ~>[_, _]: Category]:
  type X
  val u: Family2[X1, X2, X, ~>]
  def unique[Y](y: Family2[X1, X2, Y, ~>]): X ~> Y
  def universalProp[Y](y: Family2[X1, X2, Y, ~>], i: Boolean) = ???
//    val f = unique(y)
//    if i then
//      f ◦ u._1 <-> y._1
//    else
//      f ◦ u._2 <-> y._2
end Sum2

// Example: the sum of A, B in Scala
def ScalaSum2[A, B] =
  new Sum2[A, B, Scala] {
    type X = Either[A, B]
    val u =
      new Family2[A, B, X, Scala] {
        val t = ???
        val _1 = Left(_)
        val _2 = Right(_)
      }

    def unique[Y](y: Family2[A, B, Y, Scala]): X => Y =
      _.fold(y._1, y._2)
  }


def ScalaSum2b[A, B] =
  new Sum2[A, B, Scala] {

    type Xs = (A, B)

    val I: (0,1) = (0,1)

    type X = Ops.Union[Tuple.Zip[Xs, I.type]]

    summon[ X =:= ((A, 0) | (B, 1)) ]

    val inj = [Y, i] => (i: i) => (y: Y) => (y, i)


    val u =
      new Family2[A, B, X, Scala] {

        type XsI = Tuple.Zip[Xs, (0, 1)]

        summon[ XsI =:= ((A, 0), (B, 1))]

        type F[Y] = Y => ((A, 0) | (B, 1))
        type G[T <: Int] = Tuple.Elem[Xs, T] => ((A, 0) | (B, 1))

        // val a = (0, 1).map[F]( [T] => (t: T) => ((y: Tuple.Elem[Xs, T]) => (y, t)): F[Tuple.Elem[Xs, T]] )


        // val a: Arrows = ((0, 1): (0, 1)).map[G]( [T] => (t: T) => (y => (y, t): (Tuple.Elem[Xs, T], T) ): G[T] )

//        summon[ Arrows =:= (F[A], F[B]) ]
//        summon[ Arrows =:= (G[0], G[1]) ]
        summon[ Arrows =:= ( A => X, B => X) ]
//        summon[ Arrows =:= ( A => ((A, 0) | (B, 1)), B => ((A, 0) | (B, 1))) ]

        val t: Arrows = ???
//          (
//          (y => (y, 0): (Tuple.Elem[Xs, 0], 0)): G[0],
//          (y => (y, 1): (Tuple.Elem[Xs, 1], 1)): G[1]
//        )
        val _1 = ??? // t(0)
        val _2 = ??? // t(1)
      }

    def unique[Y](y: Family2[A, B, Y, Scala]): X => Y = {
      case (a, _): (A, 1) => y._1(a)
      case (b, _): (B, 2) => y._2(b)
    }
  }

// --------
// FamilyN
// --------
trait FamilyN1[Xs <: Tuple, To, ~>[_, _]] {
  val s: Int
  val ss = s + 1
  type SS = s.type
  type SSS = ss.type
  type Arrows = Tuple.Map[Xs, [Xi] =>> Xi ~> To]
  val ui: Arrows // (X1 ~> To, ..., Xn ~> To)
}

//implicit def emptyFamilyScala[To]: FamilyN1[Unit, Nothing, Scala] =
//  new FamilyN1 {
//    val s = 0
//    type Arrows = Unit
//    val ui = ()
//  }

type ToUnion[Xs <: Tuple, Is <: Tuple] =
  Ops.Union[Ops.Map[Tuple.Zip[Xs, Is], [Xi <: Tuple] =>> (Tuple.Elem[Xi, 0], Tuple.Elem[Xi, 1])]]

import compiletime.*

given stepFamilyScala[h, t <: Tuple, Xs <: Tuple, Is <: Tuple](
  using
    fn: FamilyN1[t, ToUnion[Xs, Is], Scala]
  ): FamilyN1[h *: t, (h, fn.SSS) | ToUnion[Xs, Is], Scala] =
  new FamilyN1 {
    val s: fn.SSS = fn.ss

    summon[fn.Arrows =:= Tuple.Map[     t,  [H] =>> H =>                ToUnion[Xs, Is]  ] ]
    summon[   Arrows =:= Tuple.Map[h *: t,  [H] =>> H => ((h, fn.SSS) | ToUnion[Xs, Is]) ] ]
    // Xs = (X1,)
    // Is = (a,)
    // ToUnion[Xs, Is] =:= (X1, a) | Nothing
    //
    // (h, t1, t2) =>> (
    //   h  => (h, fn.SSS) | ToUnion[Xs, Is] ,
    //   t1 => (h, fn.SSS) | ToUnion[Xs, Is] ,
    //   t2 => (h, fn.SSS) | ToUnion[Xs, Is]
    // )

    // (h, t1, t2) =>> (
    //  h  => (h, fn.SSS) | ToUnion[Xs, Is],
    //  t1 =>               ToUnion[Xs, Is],
    //  t2 =>               ToUnion[Xs, Is],
    // )

    val ui = ??? //((y: h) => (y, s)) *: fn.ui
  }



// object familyDerivationExample extends App
//   val f0 = summon[FamilyN1[Unit, Scala]]
//   val f1 = summon[FamilyN1[Int *: Unit, Scala]]
//   val ui1 = f1.ui
//   println(ui1.getClass)


trait FamilyN[Xs <: Tuple, X, ~>[_, _]] {
  // type X = Ops.Union[Tuple.Zip[Xs, I.type]]
  type Arrows = Tuple.Map[Xs, [Xi] =>> Xi ~> X]
  val s: Int
  val ui: Arrows
}

object example extends FamilyN[(Int, String), String, Scala] {
  val s = 2
  val ui = (i => (i+1).toString, _.toString)
}

trait SumN[Xs <: Tuple, ~>[_, _]: Category] {
  type X
  val u: FamilyN[Xs, X, ~>]
  def unique[Y](y: FamilyN[Xs, Y, ~>]): X ~> Y
}
  // def universalProp[Y](y: Family2[X1, X2, Y, ~>], i: Boolean) =
  //   val f = unique(y)
  //   if i then
  //     f ◦ u._1 <-> y._1
  //   else
  //     f ◦ u._2 <-> y._2


def ScalaSumN[Xs <: Tuple, Is <: Tuple] = {
  type Z = Tuple.Zip[Xs, Is]
  // Xt =:= ( (X1, 1), (X2, 2), ..., (Xn, n) )
  type Xt = Ops.Map[Z, [Xi <: Tuple] =>> (Tuple.Elem[Xi, 0], Tuple.Elem[Xi, 1])]
  new SumN[Xs, Scala] {
    type X = Ops.Union[Xt]
    val u =
      new FamilyN[Xs, X, Scala] {
        val s = ???
        // Arrows =:= (X1 => X, ..., Xn => X)
        val ui: Arrows = ???
      }
    def unique[Y](y: FamilyN[Xs, Y, Scala]): X => Y = ???
  }

}
// ----------------
// Initial objects
// ----------------

trait Initial[⊥, ~>[_, _]: Category] {
  def unique[X]: ⊥ ~> X
}

// type Union[T <: Tuple, Acc] = T match {
  //   case Unit => Acc
  //   case h *: t => Union[h, h | Acc]
  // }

