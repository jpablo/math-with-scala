package applications.zioInstances

import annotations1.Law
import categories.simple.*
import categories.simple.categoryExamples.*
import categories.simple.functor.Functor.Id
import categories.simple.categoryExamples.{_, given}
import categories.simple.monoidalCategories.{BraidedMonoidalCategory, MonoidalCategory, StrictMonoidal, CartesianMonoidalCategory}
import zio.{RIO, ZIO}
import discipline1.{<->, IsEq}
import categories.simple.functor.*

object ZIOInstances {

  // -------------------------
  // Category instance: RIOCat
  // Objects are all types
  // Morphisms are values RIO[A, B]
  // -------------------------
  given RIOCat: Category[RIO] with
    def id[A] = RIO.identity[A]
    extension [A, B, C] (g: RIO[B, C]) def ◦ (f: RIO[A, B]) = g compose f

  // --------------------------
  // Product Category RIO × RIO
  // --------------------------
  // Objects are tuples (A1, A2)
  summon[Category[RIO × RIO]]
  // Morphisms are pairs ( RIO[A,B], RIO[C,D] )
  type **>[A, B] =
    ( RIO[_1[A], _1[B]],
      RIO[_2[A], _2[B]] )
  // this is the same as (RIO × RIO)[A, B]

  // A way to mark tuples
  type Tuple[T] = (_1[T], _2[T])
  // Product[(A, B)] =:= (A, B)

  // --------------------------------
  // Product Category RIO × RIO × RIO
  // --------------------------------
  // Objects are triples (A1, A2, A3)
  // Morphisms are triples (RIO, RIO, RIO)
  type RIO3[A, B] = Prod3[RIO, RIO, RIO][A, B]
  val RIO3Cat = summon[Category[RIO3]]

  // ----------------------------
  // A functor RIO × RIO --> RIO
  // ----------------------------
  //  f: RIO[A1, B1]
  //  g: RIO[A2, B2]
  // (f, g) => f ⨂ g == f *** g : RIO[(A1, A2), (B1, B2)]
  val tensorProduct = //  (RIO × RIO --> RIO) [Tuple]
    new Functor[Tuple, RIO × RIO, RIO]:
      def map[A, B](f: A **> B) = f._1 *** f._2

  // ---------------------------------------------------------
  // A Monoidal Category instance defined using Tuple2 directly
  // ---------------------------------------------------------
  object RIOMonCat extends MonoidalCategory[RIO]:
    type ⨂[A, B] = (A, B)
    type I = EmptyTuple
    def tensor: (RIO × RIO --> RIO) [Tuple] = tensorProduct

    val associator: (Lassoc <===> Rassoc)[RIO3, RIO] =
      new NaturalIsomorphism with (Lassoc ==> Rassoc)[RIO3, RIO]:
        // (x ⨂ y) ⨂ z =:= ((x, y), z)
        val source: (RIO3 --> RIO)[Lassoc] =
          new Functor:
            def map[A, B](f: RIO3[A, B]) =
              val (f1, f2, f3) = f
              (f1 *** f2) *** f3

        // x ⨂ (y ⨂ z) =:= (x, (y, z))
        val target: (RIO3 --> RIO)[Rassoc] =
          new Functor:
            def map[A, B](f: RIO3[A, B]) =
              val (f1, f2, f3) = f
              f1 *** (f2 *** f3)

        def apply[A]   = RIO.fromFunction { case ((x, y), z) => (x, (y, z)) }
        def inverse[A] = RIO.fromFunction { case (x, (y, z)) => ((x, y), z) }

    // L[X] =  I ⨂ X
    val leftUnitor : (L <===> Id)[RIO, RIO] =
      new NaturalIsomorphism with (L ==> Id)[RIO, RIO]:
        val source: (RIO --> RIO)[L] =
          new Functor:
            def map[A, B](f: RIO[A, B]) =
              f.provideSome((a: I ⨂ A) => a._2).map(b => (EmptyTuple, b))
        val target: (RIO --> RIO)[Id] = Functor.identity[RIO]
        def apply[A] = RIO.fromFunction { _._2 }
        def inverse[A] = RIO.fromFunction { a => (EmptyTuple, a) }

    // R[X] =  X ⨂ I
    val rightUnitor: (R <===> Id)[RIO, RIO] =
      new NaturalIsomorphism with (R ==> Id)[RIO, RIO]:
        val source: (RIO --> RIO)[R] =
          new Functor:
            def map[A, B](f: RIO[A, B]) =
              f.provideSome((a: A ⨂ I) => a._1).map(b => (b, EmptyTuple))
        val target: (RIO --> RIO)[Id] = Functor.identity[RIO]
        def apply[A] = RIO.fromFunction { _._1 }
        def inverse[A] = RIO.fromFunction { a => (a, EmptyTuple) }

  // ---------------------------
  // Internal Product Instance
  // ---------------------------
  object RIOInternalProduct extends InternalProduct[RIO]:
    type *[A, B] = (A, B)
    def fst[A, B]: RIO[(A, B), A] = RIO.first
    def snd[A, B]: RIO[(A, B), B] = RIO.second
    // universal property:
    extension [A, B, C] (f: RIO[C, A]) def * (g: RIO[C, B]): RIO[C, (A, B)] = f &&& g
    type T = EmptyTuple
    val terminal =
      new Terminal:
        def arrow[Y]: RIO[Y, EmptyTuple] = RIO.succeed(EmptyTuple)


  // -----------------------------------------------------------------------
  // A Monoidal Category instance defined using the InternalProduct instance
  // -----------------------------------------------------------------------
  implicit val RIOMonCatProduct: CartesianMonoidalCategory[RIO, Tuple2] =
    new CartesianMonoidalCategory with MonoidalCategory[RIO] with InternalProduct[RIO]:
      // ----------------------------
      // Internal Product definitions
      // ----------------------------
//      type *[A, B] = (A, B)
      def fst[A, B]: RIO[(A, B), A] = RIO.first
      def snd[A, B]: RIO[(A, B), B] = RIO.second
      // universal property: A <~ X ~> B   ==>   X ~> (A, B)
      extension [A, B, X] (f: RIO[X, A]) def * (g: RIO[X, B]): RIO[X, (A, B)] = f &&& g
      type T = EmptyTuple
      val terminal =
        new Terminal:
          def arrow[Y]: RIO[Y, EmptyTuple] = RIO.succeed(EmptyTuple)
      // -----------------------------
      // Monoidal Category definitions
      // -----------------------------
//      type ⨂[A, B] = A * B
      type I = T
      def tensor: Functor[Tuple, RIO × RIO, RIO] = tensorProduct

      val associator: (Lassoc <===> Rassoc)[RIO3, RIO] =
        new NaturalIsomorphism with (Lassoc ==> Rassoc)[RIO3, RIO](using RIO3Cat, RIOCat):
          // (x ⨂ y) ⨂ z =:= ((x, y), z)
          val source: (RIO3 --> RIO)[Lassoc] =
            new Functor:
              def map[A, B](f: RIO3[A, B]) =
                val (f1, f2, f3) = f
                (f1 ** f2) ** f3
          // x ⨂ (y ⨂ z) =:= (x, (y, z))
          val target: (RIO3 --> RIO)[Rassoc] =
            new Functor:
              def map[A, B](f: RIO3[A, B]) =
                val (f1, f2, f3) = f
                f1 ** (f2 ** f3)

          def apply[A] =
            RIO.fromFunction { case ((x, y), z) => (x, (y, z)) }

          def inverse[A] =
            RIO.fromFunction { case (x, (y, z)) => ((x, y), z) }

      val leftUnitor : (L <===> Id)[RIO, RIO] =
        new NaturalIsomorphism with (L ==> Id)[RIO, RIO]:
          val source: (RIO --> RIO)[L] =
            new Functor:
              def map[A, B](f: RIO[A, B]) =
                f.provideSome((a: I * A) => a._2).map(b => (EmptyTuple, b))
          val target: (RIO --> RIO)[Id] = Functor.identity[RIO]
          def apply[A] =
            RIO.fromFunction { _._2 }
          def inverse[A] =
            RIO.fromFunction { a => (EmptyTuple, a) }

      val rightUnitor: (R <===> Id)[RIO, RIO] =
        new NaturalIsomorphism with (R ==> Id)[RIO, RIO]:
          val source: (RIO --> RIO)[R] =
            new Functor:
              def map[A, B](f: RIO[A, B]) =
                f.provideSome((a: A ⨂ I) => a._1).map(b => (b, EmptyTuple))
          val target: (RIO --> RIO)[Id] =
            Functor.identity[RIO]
          def apply[A] =
            RIO.fromFunction { _._1 }
          def inverse[A] =
            RIO.fromFunction { a => (a, EmptyTuple) }

  // -------------------------
  // Braided Monoidal instance
  // -------------------------
//  given RIOBraidedCat: BraidedMonoidalCategory[RIO]:
//    val braiding: (Tensor <===> Braiding)[RIO, RIO] = ???
}
