package categories.simple

import discipline1.*
import annotations1.*

/*                *
 * Isomorphisms   *
 *                */

class Isomorphism[X, Y, ~>[_, _]](
  val from : X ~> Y,
  val to   : Y ~> X
)(using C: Category[~>]):
  // f: X ~> Y is an isomorphism if there exists an arrow g such that
  @Law
  def IdA = (to ◦ from) <-> C.id[X]

  @Law
  def IdB = (from ◦ to) <-> C.id[Y]


case class Bijection[A, B](
  override val from: A => B, 
  override val to: B => A
) extends Isomorphism(from, to)

type ≅[A, B] = Bijection[A, B]
