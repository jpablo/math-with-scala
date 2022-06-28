package categories.simple

import annotations1.*
import discipline1.{IsEq, <->}


trait Category0[Hom[_, _]]:
  type ~>[A, B] = Hom[A, B]
  
  def id[A]: A ~> A

  extension [A, B, C] (g: B ~> C)
    def ◦ (f: A ~> B): A ~> C = f >>> g
  
  extension [A, B, C] (f: A ~> B)
    def >>> (g: B ~> C): A ~> C
  

  @Law
  def associativity[A, B, C, D](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    h ◦ (g ◦ f) <-> (h ◦ g) ◦ f

  @Law
  def identityR[A, B](f: A ~> B) = f ◦ id[A] <-> f
  
  @Law
  def identityL[A, B](f: A ~> B) = id[B] ◦ f <-> f



trait Category1[Hom[_, _]]:
  type ~>[A, B] = Hom[A, B]

  type Id[A]
  type ◦[A, B]
  type >>>[A, B] = ◦[B, A]

  def id[A]: Id[A]

  extension [A, B, C] (g: B ~> C)
    def ◦ (f: A ~> B): A ~> C = f >>> g

  extension [A, B, C] (f: A ~> B)
    def >>> (g: B ~> C): A ~> C


  @Law
  def associativity[f, g, h]:
    h ◦ (g ◦ f) =:= (h ◦ g) ◦ f

  @Law
  def identityR[A, f]:
    (f ◦ Id[A]) =:= f

  @Law
  def identityL[B, f]:
    (Id[B] ◦ f) =:= f


trait InternalProduct1[~>[_, _]] extends Category1[~>]:
  // A product of two objects A and B is an object A * B
  type *[A, B]

  type Fst
  type Snd

  // together with a pair of arrows
  def fst[A, B]: (A * B) ~> A
  def snd[A, B]: (A * B) ~> B

  // such that for any pair of arrows
  //    f   g
  // A <~ C ~> B
  // there is exactly one arrow (f * g): C ~> (A × B)
  extension [A, B, C]
    (f: C ~> A) def * (g: C ~> B): C ~> (A * B)


  def factorization1[A,B,C](
    f: C ~> A,
    h: C ~> (A * B)
  ) =
    fst ◦ h <-> f

  def factorization2[A,B,C](
    g: C ~> B,
    h: C ~> (A * B)
  ) =
    snd ◦ h <-> g

  // such that
  @Law
  def projectionFst[A, B, C](f: C ~> A, g: C ~> B) =
    // factorization1(f, f * g) ==
    fst ◦ (f * g) <-> f

  @Law
  def projectionSnd[A, B, C](f: C ~> A, g: C ~> B) =
    // factorization2(g, f * g) ==
    snd ◦ (f * g) <-> g

  // or in other words: for any arrow h
  @Law
  def uniqueness[A, B, C](
    f: C ~> A,
    g: C ~> B,
    h: C ~> (A * B)
  ) =
    // such that
    // factorization1(f, h)
    // factorization2(g, h)
    // then
    h <-> f * g

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
  // def Δ[A]: A ~> (A * A) =
  //   Cat.id[A] * Cat.id[A]
  // // -----------------
  // // deletion morphism
  // // -----------------
  // def ![A]: A ~> T =
  //   terminal.unique[A]

