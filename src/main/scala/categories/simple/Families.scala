package categories.simple

import discipline1.*
import categories.simple.{_, given}


trait Family[From, To[_ <: Int], ~>[_, _]: Category] {

  // TODO: this is not typesafe!
  // consider indexing by type and requiring the
  // value as an implicit argument
  def p(i: Int): From ~> To[i.type]

  // helper
  def apply(i: Int) = p(i)
}

// The product of a family F[_]
trait Prod[P, F[_ <: Int], ~>[_, _]: Category] extends Family[P, F, ~>] {

  def unique[Y](y: Family[Y, F, ~>]): Y ~> P

  def universalProp[Y](y: Family[Y, F, ~>], i: Int): IsEq[Y ~> F[i.type]] = ???
//    val f = unique(y)
//    p(i) ◦ f <-> y(i)
}

// example: tuples
def tupleProd[T <: NonEmptyTuple] = {

  type F[Tu <: NonEmptyTuple] =
    [i <: Int] =>> Tuple.Elem[Tu, i]

  new Prod[T, F[T], Scala] with Family[T, F[T], Scala] {
    def p(i: Int): T => Tuple.Elem[T, i.type] = _(i)
    // don't know how to implement this for general tuples
    def unique[Y](y: Family[Y, F[T], Scala]): Y => T = ???
  }
}

// Example for a 2 tuple:

def tuple2Prod[A, B] = {
  type AB = (A, B)

  type F[t <: Tuple] =
    [i <: Int] =>> Tuple.Elem[t, i]

  new Prod[AB, F[AB], Scala] with Family[AB, F[AB], Scala] {

    def p(i: Int): AB => Tuple.Elem[AB, i.type] = _(i)

    def unique[Y](yi : Family[Y, F[AB], Scala]): Y => AB =
      y => ( yi(0)(y), yi(1)(y) )
  }
}


object ProdApp extends App {
  val prodIntString = tuple2Prod[Int, String]
  val p0: ((Int, String)) => Int    = prodIntString(0)
  val p1: ((Int, String)) => String = prodIntString(1)
  val t = (1, "a")
  println(p0(t))
  println(p1(t))
  // throws an exception!!
//  val p2 = prodIntString(2)
//  println(p2(t))
}

// ------------------
// type safe families
// ------------------
// Note: Dec 31  2019: Does not compile!

// trait Family1[From, To[_ <: Int], ~>[_, _]: Category]
//   def p[i <: Int](using v: ValueOf[i]): From ~> To[i]
//   def apply[i <: Int](using v: ValueOf[i]) = p[i]


// trait Prod1[P, F[_ <: Int], ~>[_, _]: Category] extends Family1[P, F, ~>]
//   def unique[Y](y: Family1[Y, F, ~>]): Y ~> P
//   def universalProp[Y, i <: Int: ValueOf](y: Family1[Y, F, ~>]): IsEq[Y ~> F[i]] =
//     val f = unique(y)
//     p[i] ◦ f <-> y[i]

// def tuple2Prod1[A, B] =
//   type AB = (A, B)
//   type F[t <: Tuple] = [i <: Int] =>> Tuple.Elem[t, i]
//   new Prod1[AB, F[AB], Scala] with Family1[AB, F[AB], Scala] with
//     // TODO: try with newer dotty versions. Does not compile.
//     def p[i <: Int](using v: ValueOf[i]): AB => Tuple.Elem[AB, i] =
//       (t: AB) => t.apply(v.value)

//     def unique[Y](yi : Family1[Y, F[AB], Scala]): Y => AB = ???
//       // y => ( yi(0)(y), yi(1)(y) )




