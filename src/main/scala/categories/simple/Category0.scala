package categories.simple

import annotations1.*
import isEqual.{IsEq, <->}


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

