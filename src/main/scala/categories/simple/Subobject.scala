package categories.simple

import annotations1.*
import categories.simple.*
import discipline1.*


case class Monic[A, B, ~>[_, _]: Category](f: A ~> B) {

  @Law
  def monic[C](g: C ~> A, h: C ~> A)(using CanEqual[C ~> B, C ~> B], CanEqual[C ~> A, C ~> A]) =
    if (f ◦ g == f ◦ h) then g == h else true

//    // How can we express at the type level the fact that f ◦ g == f ◦ h ?
//    val fg = f ◦ g
//    val fh = f ◦ h
//    val prop:
//      fh.type =:= fh.type => g.type =:= h.type = ???
//

}


// A subobject of an object D is a monic arrow
case class Subobject[A, D, ~>[_, _]: Category](f: Monic[A, D, ~>])


trait Inclusion[D, ~>[_, _]: Category, A, B] {
  val f: A ~> D
  val g: B ~> D
  def h: A ~> B

  @Law
  def prop =
    f <-> g ◦ h
}