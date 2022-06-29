package algebra

import discipline1.*
import annotations1.*

trait Group[G] extends Monoid[G]:

  extension (a: G) def inv: G

  @Law
  def inverseLeft(x: G) =
    x * x.inv <-> id

  @Law
  def inverseRight(x: G) =
    x.inv * x <-> id

  @Law
  def prod(x: G, n: Int): G =
    n match {
      case j if j < 0 => x.inv * prod(x.inv, n - 1)
      case 0          => id
      case j if j > 0 => x * prod(x, n - 1)
    }

object Group:
  def apply[G](using G: Group[G]) = G

  type Hom[A, B] = Homomorphism[Group, A, B]

  @Law
  def multiplication[A: Group, B: Group](
    f: Hom[A, B],
    x: A,
    y: A
  ) =
    f(x * y) <-> f(x) * f(y)

trait AbelianGroup[A] extends Group[A]:
  @Law
  def commutativity(x: A, y: A) =
    x * y <-> y * x


def inf[A](p: A => Boolean): A = ???
def sup[A](p: A => Boolean): A = ???

// the order of a group x: G
def order[G: Group](x: G): Int =
  sup { n => Group[G].prod(x, n) == Group[G].id }

// group actions
trait GroupAction[G: Group, A]:

  extension (g: G) def ⋅ (a: A): A

  def apply(g: G): A => A = g ⋅ _

  def property1(g1: G, g2: G, a: A) =
    g1 ⋅ (g2 ⋅ a) <-> (g1 * g2) ⋅ a

  def property2(a: A) =
    Group[G].id ⋅ a <-> a


import categories.simple.Bijection

def sigmaIsPermutation[G: Group, A](g: G, σ: GroupAction[G, A]): Bijection[A, A] =
  val G = Group[G]
  import G.inv
  import σ.⋅

  val f1 = σ(g.inv) compose σ(g)
  val f2 = σ(g)     compose σ(g.inv)

  // `f1` is the identity on A
  def proof1(a: A): List[IsEq[A]] =
    List(
      f1(a) <-> (σ(g.inv) compose σ(g))(a),
      f1(a) <-> g.inv ⋅ (g ⋅ a),
      f1(a) <-> (g.inv * g) ⋅ a,
      f1(a) <-> G.id ⋅ a,
      f1(a) <-> a,
    )
  // the proof that `f2` is also the identity on A is identical

  Bijection(σ(g), σ(g.inv))

