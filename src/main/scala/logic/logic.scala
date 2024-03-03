package logic

trait Prop

// ------------------
// true / false
// ------------------

/**
 * A type with a single inhabitant. A tautology
 */
case object True extends Prop
type True = True.type

/**
 * A type without any inhabitant
 */
sealed trait False extends Prop

object False:
  /**
   * This function creates a value of an arbitrary type `C` given a contraction.
   *
   * TODO: Understand this better!
   */
  def elim[C](h: False): C = throw new Exception("Impossible!")

//-------------
// Conjuntion
//-------------
infix case class and[A <: Prop, B <: Prop](left: A, right: B) extends Prop

type ∧[A <: Prop, B <: Prop] = A and B

extension [A <: Prop, B <: Prop]  (left: A)
  def ∧ (right: B): A ∧ B = and(left, right)

trait AndExamples {
  type P <: Prop
  type Q <: Prop
  def ex1 (h: P ∧ Q): Q ∧ P = h.right ∧ h.left
  def ex2 (h: P ∧ Q): Q ∧ P ∧ Q = h.right ∧ h.left ∧ h.right
}


// -----------
// Disjunction
// -----------
infix sealed trait or[P <: Prop, Q <: Prop] extends Prop

type ∨[P <: Prop, Q <: Prop] = P or Q

object or:
  // intro_left
  case class inl[P <: Prop, Q <: Prop](hp: P) extends (P ∨ Q)
  // intro_right
  case class inr[P <: Prop, Q <: Prop](hp: Q) extends (P ∨ Q)

  def elim[P <: Prop, Q <: Prop, R <: Prop](h1: P ∨ Q)(h2: P => R, h3: Q => R): R =
    h1 match
      case inl(h) => h2(h)
      case inr(h) => h3(h)
end or

trait OrExamples:
  type P <: Prop
  type Q <: Prop

  def ex1 (h: P): P ∨ Q = or.inl(h)

  def ex2 (h: Q): P ∨ Q = or.inr(h)

  def ex3 (h: P ∨ Q): Q ∨ P =
    or.elim(h)(or.inr.apply, or.inl.apply)

end OrExamples

// -----------
// Negation
// -----------

// In Lean, functions producing props are props!
// To simulate this in Scala we create a wrapper type for functions that also extends `Prop`
class PropFunction[A, P <: Prop](f: A => P) extends (A => P) with Prop:
  def apply(a: A) = f(a)

type not[a <: Prop] = PropFunction[a, False]

// helper function
def not[P <: Prop](contradiction: P => False): not[P] =
  PropFunction(contradiction)

type ¬[P <: Prop] = not[P]


/**
 * Can't have both P and not P
 */
def absurd[P <: Prop, B](p: P, notP: not[P]): B =
  False.elim(notP(p))

trait NotExamples {

  type P <: Prop
  type Q <: Prop

  // P and not P => contradiction!
  def ex0 (p: P, np: not[P]): False =
    np(p)

  summon[ not[P] <:< Prop ]

  // (p => q) => (¬q => ¬p)
  def ex1 (pq: P => Q, notQ: not[Q]): not[P] =
    not(pq andThen notQ)

  // p and not p implies everything
  def ex2 (p: P, notP: not[P]): Q = False.elim(notP(p))
}


// -----------
// Equivalence
// -----------

// mp: modus ponens
infix case class iff[A <: Prop, B <: Prop](mp: A => B, mpr: B => A) extends Prop

type <->[A <: Prop, B <: Prop] = A iff B

extension [A <: Prop, B <: Prop]  (mp: A => B)
  def <-> (mpr: B => A): A <-> B = iff(mp, mpr)

object iff:
  // iff.rec : Π {a b : Prop} {C : Sort u_1}, ((a → b) → (b → a) → C) → (a ↔ b) → C
  def elim[A <: Prop, B <: Prop, C <: Prop]: ((A => B) => (B => A) => C) => (A <-> B) => C = ???

  // just an alias
  def intro[A <: Prop, B <: Prop](mp: A => B, mpr: B => A) = mp <-> mpr


// ---------------
// Classical Logic
// ---------------

object Classical:
  /**
   * Law of Excluded Middle: ∀ (p : Prop), p ∨ ¬p
   */
  def em[P <: Prop]: P ∨ not[P] = throw new Exception("seriously?")

  /**
   *  {p : Prop} (h : ¬¬p) : p
   *  This allows to create a proof by contradiction:
   *  To prove P, assume not[P] and derive False
   */
  def doubleNegationElimination[P <: Prop](nnP: not[not[P]]): P =
    or.elim(em[P])(
      // if P is true, just return the proof p
      (p: P) => p,
      // if not[p], use absurd to "create" a P
      (nP: not[P]) => absurd(nP, nnP): P
    )


trait DistributivityExample {
  type P <: Prop
  type Q <: Prop
  type R <: Prop

  // (P and (Q or R)) <-> (P and Q) or (Q and R)
  def distributivity: (P ∧ (Q ∨ R)) <-> (P ∧ Q) ∨ (Q ∧ R) =
    iff.intro(
      (h: P ∧ (Q ∨ R)) => {
        val hp: P = h.left
        or.elim(h.right)(
          (hq: Q) => or.inl(hp),
          (hr: R) => or.inr(hr)
        )
        ???
      },
      ???
    )
}


// ----------------------
// Universal quantifier
// ----------------------
trait UniversalQuantifierExamples:


  // Example:

  // variables (α : Type*) (p q : α → Prop)
  //
  //  example: (∀ (x : α), p x ∧ q x ) -> ∀ (x : α), p x :=
  //    λ h x, (h x).left

  // given
  //  val h: [X] => (p: X => Prop, q: X => Prop, x: X) => p(x) and q(x)
  // produce
  // (y: X) => p(y)

  // option 1: x is a term, use its singleton type
  // [A, P: A =>> Prop, Q: A =>> Prop] => (x: A) => P[x.type] and Q[x.type]
  val h1: [A, P[_ <: A] <: Prop, Q[_ <: A] <: Prop] => (x: A) => P[x.type] and Q[x.type] = ???

  // option 2: x is a type
  val h2: [A, P[_ <: A] <: Prop, Q[_ <: A] <: Prop, X <: A] => () => P[X] and Q[X] = ???


  // 1: singleton types
  def ex1a[A, P[_ <: A] <: Prop, Q[_ <: A] <: Prop]: ((x: A) => P[x.type] and Q[x.type]) => ((y: A) => P[y.type]) =
    h => a => h(a).left

  // 2: term of a subtype X <: A
  def ex1b[A, P[_ <: A] <: Prop, Q[_ <: A] <: Prop, X <: A]: (X => P[X] and Q[X]) => (X => P[X]) =
    h => x => h(x).left

  // 3: term of type A
  def ex1c[A, P[_ <: A] <: Prop, Q[_ <: A] <: Prop]: (A => P[A] and Q[A]) => (A => P[A]) =
    h => x => h(x).left


// ----------------------
// Existential quantifier
// ----------------------

// inductive Exists {α : Sort u} (p : α → Prop) : Prop
// | intro (w : α) (h : p w) : Exists

// version 1
class Exists[A, P[_ <: A] <: Prop](w: A, h: P[w.type]) extends Prop

object Exists {
  def intro[A, P[_ <: A] <: Prop](w: A, h: P[w.type]) = Exists(w, h)
}

// version 2
class Exists2[A, P[_ <: A] <: Prop, W <: A](h: P[W]) extends Prop


// -----------
// Equality
// -----------

enum Eq[A, B] extends Prop:
  case reflexivity() extends Eq[A, A]
  case reflexivityV[X](x: X) extends Eq[X, X]

type ==[A, B] = Eq[A, B]

object Eq {
  def refl[A]: A == A = Eq.reflexivity()
//  def reflV[A](a: A, b: A)(using a.type =:= b.type): a.type == b.type = Eq.reflexivity()

  val f: Int => Int = ???
  val g: Int => Int = ???
  type FG = f.type == g.type
}


object EqExamples extends App {
  val ex1: 1 == 1 = Eq.refl
  val ex2: 1 == 2 = ???

  println(ex1)
  import scala.compiletime.ops.int.*

  def lemma_add_succ[n <: Int]: n + 0 == n = ???

  def lemma_zero_add2[n <: Int]: 0 + n == n = ???

  def lemma_zero_add(n: Int): 0 + n.type == n.type = {
    type t = 0 + n.type
//    Eq.refl[0 + n.type]
    ???
  }


  summon[ 0 + 0 =:= 0 ]

  val zz: 0 + 0 == 0 = Eq.refl
  // subtypes

  case class Subtype[A, P[_ <: A] <: Prop]
    (value: A)
    (property: P[value.type])
}

