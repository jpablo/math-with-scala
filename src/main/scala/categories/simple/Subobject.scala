package categories.simple

import annotations1.*
import categories.simple.*
import isEqual.*


case class Monic[A, B, ~>[_, _]: Category](f: A ~> B):
  @Law
  def monic[C](g: C ~> A, h: C ~> A) =
    if (f ◦ g == f ◦ h) then g == h else true


// A subobject of an object D is a monic arrow
case class Subobject[A, D, ~>[_, _]: Category](f: Monic[A, D, ~>])


trait Inclusion[D, ~>[_, _]: Category, A, B]:
  val f: A ~> D
  val g: B ~> D
  def h: A ~> B

  @Law
  def prop =
    f <-> g ◦ h
