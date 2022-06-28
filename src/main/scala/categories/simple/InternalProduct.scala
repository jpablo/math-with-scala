package categories.simple

import discipline1.*
import categories.simple.*
import annotations1.*
import cats.data.AndThen

// Internal Categorical Product
trait InternalProduct[~>[_, _]](using val Cat: Category[~>]):
//  import Cat.AndThenC

  // A product of two objects A and B is an object A * B
  type *[A, B]

  // together with a pair of arrows
  def fst[A, B]: (A * B) ~> A
  def snd[A, B]: (A * B) ~> B

  type Fst[A, B] <: (A * B) ~> A
  type Snd[A, B] <: (A * B) ~> B

  // (Arrow constructors)

  // such that for any pair of arrows
  // A <~f~ C ~g~> B
  // there is exactly one arrow (f * g): C ~> (A × B)
  extension [A, B, C]
    (f: C ~> A) def * (g: C ~> B): C ~> (A * B)

//  type ProdT[A, B, C, F <: C ~> A, G <: C ~> B] <: C ~> (A * B)

  // such that
  @Law def prop1[A, B, C](f: C ~> A, g: C ~> B) = (f * g) >>> fst <-> f
  @Law def prop2[A, B, C](f: C ~> A, g: C ~> B) = (f * g) >>> snd <-> g

//  def prop2T[A, B, C, F <: C ~> A, G <: C ~> B]
//  : AndThenC[C, A * B, B, ProdT[A, B, C, F, G], Snd[A, B]] =:= G = ???

  @Law def uniqueness[A, B, C] (
    f: C ~> A,
    g: C ~> B,
    h: C ~> (A * B),
  )(using
    CanEqual[C ~> A, C ~> A],
    CanEqual[C ~> B, C ~> B],
  ): IsEq[C ~> (A * B)] =  {
    assert(h >>> fst == f)
    assert(h >>> snd == g)
    h <-> f * g
  }

  // and a terminal object
  type T
  val terminal: Terminal[T, ~>]

  // ------------
  // Product maps
  // ------------
  // f ** g : (A1 * A2) ~> (B1 * B2)
  extension [A1, A2, B1, B2]
    (f: A1 ~> B1) def ** (g: A2 ~> B2): (A1 * A2) ~> (B1 * B2) =
      (f ◦ fst) * (g ◦ snd)
  // --------------------
  // duplication morphism
  // --------------------
  def Δ[A]: A ~> (A * A) =
    Cat.id[A] * Cat.id[A]
  // -----------------
  // deletion morphism
  // -----------------
  def ![A]: A ~> T =
    terminal.arrow[A]

end InternalProduct

given CartesianProduct: InternalProduct[Scala] with
  type *[A, B] = (A, B)

  extension [A, B, C] (f: C => A) def * (g: C => B): C => (A, B) =
    c => (f(c), g(c))

  def fst[A, B] = _._1
  def snd[A, B] = _._2

  type T = EmptyTuple

  val terminal = new Terminal { def arrow[Y] = _ => EmptyTuple }

trait Exercises[~>[_, _]: Category: InternalProduct] {
  val C = summon[Category[~>]]
  val P = summon[InternalProduct[~>]]
  import P.{fst, snd, `*`}
//  import P.{fst, snd, `*`, projectionFst, projectionSnd, uniqueness}
  import C.id

  // -----------------------------------
  // fst[A, B] * snd[A, B] == id[A * B]
  // -----------------------------------
  def exercise_1[A, B, C](using
    CanEqual[(A * B) ~> A, (A * B) ~> A],
    CanEqual[(A * B) ~> B , (A * B) ~> B]
  ) = {
    // C.id[A * B] <-> fst[A, B] * snd[A, B]
    val uniqueness = P.uniqueness(fst[A, B], snd[A, B], C.id[A * B])


    // consider the product of the projections
    fst * snd : (A * B) ~> (A * B)
    // but the identity on A × B is another such arrow
    id[A * B] : (A * B) ~> (A * B)
    // -----------
    // we know that
    fst ◦ id[A * B] <-> fst
    snd ◦ id[A * B] <-> snd
    // since id[A × B] satisfies the projection laws, it must the the projection
    id[A * B] <-> fst * snd
    // -----------
    // or
//    uniqueness(id[A * B], fst, snd)

    fst[A, B] * snd[A, B] <-> id[A * B]
  }

  // ----------------------------------------
  // if f × g == k × h then f == k and g == h
  // ----------------------------------------
  def exercise_2[A, B, C](f: C ~> A, g: C ~> B)(k: C ~> A, h: C ~> B)
    (using CanEqual[C ~> (A * B),  C ~> (A * B)], CanEqual[C ~> A, C ~> A], CanEqual[C ~> B, C ~> B])
  = {
    f * g == k * h
    fst ◦ (f * g) ==  fst ◦ (k * h)
    f == k
    snd ◦ (f * g) ==  snd ◦ (k * h)
    g == h
  }

  // --------------------------------
  // (f ◦ h) * (g ◦ h) == (f * g) ◦ h
  // --------------------------------
  def exercise_3[A, B, C, D](f: C ~> A, g: C ~> B, h: D ~> C)(using
    CanEqual[D ~> A, D ~> A],
    CanEqual[D ~> B, D ~> B]
  ) = {
    // we know that
    (f ◦ h) * (g ◦ h) : D ~> (A * B)
    // is the unique arrow D ~> A × B such that
    // projections(f ◦ h, g ◦ h)
    // i.e.
    fst ◦ ((f ◦ h) * (g ◦ h)) == f ◦ h
    snd ◦ ((f ◦ h) * (g ◦ h)) == g ◦ h
    // thus if we show that (f × g) ◦ h  also satisfies
    // this property then by uniqueness we're done.
    // fst:
    fst ◦ ((f * g) ◦ h) == (fst ◦ (f * g)) ◦ h
    fst ◦ ((f * g) ◦ h) == f ◦ h
    // snd:
    snd ◦ ((f * g) ◦ h) == (snd ◦ (f * g)) ◦ h
    snd ◦ ((f * g) ◦ h) == g ◦ h
  }


  // ----------
  // A × ⊤ ≅ A
  // ----------
  def exercise_4[⊤, A](using T: Terminal[⊤, ~>]) = {
      import T.I
      val proof = new Isomorphism[A, A * ⊤, ~>](
        from = id[A] * I[A],
        to = fst
      )
      // since
      val a1 =
        fst ◦ (id[A] * I[A]) <-> id[A]
      // (by definition of fst)
      // and
      val a2 =
        List(
          (id[A] * I[A]) ◦ fst <-> (id[A] ◦ fst) * (I[A] ◦ fst), // by ex3
          (id[A] * I[A]) ◦ fst <-> fst * (I[A] ◦ fst),           // by id laws
          // since I[A] ◦ fst: A × ⊤ ~> ⊤, by uniqness, then
          // I[A] ◦ fst == snd
          (id[A] * I[A]) ◦ fst <-> fst * snd,
          (id[A] * I[A]) ◦ fst <-> id[A * ⊤],                    // by ex1
        )
  }


  private def exercice_5[A, B] = {
    // ---- goal --------
//    id[A] ** id[B] <-> id[A * B]
    // ------------------
    // by definition of **
//    id[A] ** id[B] <-> (id[A] ◦ fst) * (id[B] ◦ snd)
//    // setting
//    val f = id[A] ◦ fst: (A * B) ~> A
//    val g = id[B] ◦ snd: (A * B) ~> B
//    // we have
//    id[A] ** id[B] <-> f * g
//
//    // where f * g is the unique arrow (A * B) ~> (A * B) satisfying
////    projectionFst(f, g) == fst ◦ (f * g) <-> f
////    projectionSnd(f, g) == snd ◦ (f * g) <-> g
//    // (this is the universal property of the product!)
//    // on the other hand the identity id[A * B] also satifies
//    fst ◦ id[A * B] <-> f
//    snd ◦ id[A * B] <-> g
//
//    // thus by uniqueness
//    // uniqueness(id[A * B], f, g) ==
//     id[A * B] <-> f * g
//     // and
//     id[A * B] <-> id[A] ** id[B]
  }

  // A * B ≈ B * A
  def exercice_6[A, B]: Isomorphism[A * B, B * A, ~>] =

    val from: (A * B) ~> (B * A) = snd[A, B] * fst[A, B]

    val to  : (B * A) ~> (A * B) = snd[B, A] * fst[B, A]
    // p.d. to ◦ from <-> id[A * B]
    // p.d. from ◦ to <-> id[B * A]
    to ◦ from <-> (snd[B, A] * fst[B, A]) ◦ from
    // by ex 3: (f ◦ h) * (g ◦ h) == (f * g) ◦ h
    to ◦ from <-> ((snd[B, A] ◦ from) * (fst[B, A] ◦ from))

    to ◦ from <-> ((snd[B, A] ◦ (snd * fst)) * (fst[B, A] ◦ (snd * fst)))
    // by projectionSnd(snd[A, B], fst[A, B])
    to ◦ from <-> (fst * (fst[B, A] ◦ (snd * fst)))
    // by projectionFst(snd[A, B], fst[A, B])
    to ◦ from <-> (fst * snd)
    // by ex 1
    to ◦ from <-> id[A * B]


    new Isomorphism(from , to)

  // (A * B) * C ≈ A * (B * C)
  def exercise_7[A, B, C] = {

    type L = (A * B) * C
    type R =  A * (B * C)

    // ---------------------------------------
    val a: L ~> A = fst[A * B, C] >>> fst
    val b: L ~> B = fst[A * B, C] >>> snd
    val c: L ~> C = snd[A * B, C]

    val from: L ~> R = a * (b * c)
    // ---------------------------------------
    val x: R ~> A = fst[A, B * C]
    val y: R ~> B = snd[A, B * C] >>> fst
    val z: R ~> C = snd[A, B * C] >>> snd

    val to: R ~> L = (x * y) * z
    // ---------------------------------------
    // p.d. from >>> to <-> id[(A * B) * C]
    from >>> to : L ~> L

    ((fst[A * B, C] >>> fst) * ((fst[A * B, C] >>> snd) * snd[A * B, C])) >>>
    ((fst[A, B * C] * (snd[A, B * C] >>> fst)) * (snd[A, B * C] >>> snd))


  }

}


