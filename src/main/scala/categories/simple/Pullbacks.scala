package categories.simple

import discipline1.*
import annotations1.*


// A pullback of a pair A ~> C <~ B of C arrows
trait Pullback[A, B, C, ~>[_, _]: Category](
  val f: A ~> C,
  val g: B ~> C,
) {
  // is a limit for the diagram A ~> C <~ B:
  // A cone
  type D
  val fp: D ~> B
  val gp: D ~> A

  @Law
  def commutativity = f ◦ gp <-> g ◦ fp

  // with the universal property
  @Law
  def unique[E](h: E ~> A, j: E ~> B)(using CanEqual[E ~> C,E ~> C]): Option[E ~> D] =
    if (f ◦ h == g ◦ j)
    then Some(uniqueImpl[E])
    else None

  protected def uniqueImpl[E]: E ~> D

  @Law
  def uniqueImplCommutativity[E](h: E ~> A, j: E ~> B) = ???
    List(
//      gp ◦ uniqueImpl[E] <-> h,
//      fp ◦ uniqueImpl[E] <-> j,
    )
}
