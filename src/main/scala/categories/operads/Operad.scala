package categories.operads

import isEqual.{IsEq, <->}
import annotations1.*

trait Operad[Hom[_ <: Tuple, _]]:
  type ~> [A <: Tuple, B] = Hom[A, B]

  def id[A <: Tuple]: A ~> A
  
  extension [A <: Tuple, B <: Tuple, C] (f: A ~> B) def >>> (g: B ~> C): A ~> C
  
  extension [A <: Tuple, B <: Tuple, C] (g: B ~> C) def ◦ (f: A ~> B): A ~> C = f >>> g


  @Law
  def associativity[A <: Tuple, B <: Tuple, C <: Tuple, D](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    h ◦ (g ◦ f) <-> (h ◦ g) ◦ f

  @Law
  def identityL[A <: Tuple, B <: Tuple](f: A ~> B) =
    (id[B] ◦ f) <-> f

  @Law
  def identityR[A <: Tuple, B](f: A ~> B) =
    (f ◦ id[A]) <-> f

end Operad
