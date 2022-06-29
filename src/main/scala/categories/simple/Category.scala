package categories.simple

import discipline1.{IsEq, <->}
import annotations1.*


trait CategoryS[U, Hom[_ <: U, _ <: U]]:
  type ~> [A <: U, B <: U] = Hom[A, B]
  type <~ [A <: U, B <: U] = Hom[B, A]

  // Arrow constructor
  def id[A <: U]: A ~> A

  // Arrow combinator
  extension [A <: U, B <: U, C <: U] (g: B ~> C)
    def ◦ (f: A ~> B): A ~> C

  extension [A <: U, B <: U, C <: U] (f: A ~> B)
//    @targetName("andThen")
    def >>> (g: B ~> C): A ~> C = g ◦ f

  @Law
  def associativity[A <: U, B <: U, C <: U, D <: U](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    h ◦ (g ◦ f) <-> (h ◦ g) ◦ f

  @Law
  def identityL[A <: U, B <: U](f: A ~> B) =
    (id[B] ◦ f) <-> f

  @Law
  def identityR[A <: U, B <: U](f: A ~> B) =
    f ◦ id[A] <-> f

end CategoryS


type Category[Hom[_, _]] = CategoryS[Any, Hom]


object Category:
  def apply[C[_, _]](using C: Category[C]) = C


// Example on how to use the laws defined above
//class CategoryTests[Hom[_, _]](using C: Category[Hom]) extends Laws {
//
//  def laws = C
//
//  def category[A, B, C, D](using
//    arbAB: Arbitrary[Hom[A, B]],
//    arbBC: Arbitrary[Hom[B, C]],
//    arbCD: Arbitrary[Hom[C, D]],
//    eqAB: Eq[Hom[A, B]],
//    eqAD: Eq[Hom[A, D]]
//  ): RuleSet =
//    new DefaultRuleSet(
//      name = "category",
//      parent = None,
//      "right identity" -> forAll(laws.identityAxiom[A, B] _),
//      "left identity"  -> forAll(laws.identityAxiom[A, B] _),
//      "associativity"  -> forAll(laws.identityAxiom[A, B, C, D] _)
//    )
//}

// Note: This definition doesn't allow to have any restrictions on the
// objects, hence Ob(C) is the set of all types.


//========================================================================================
/*                                                                                      *
 * Category of types and pure functions                                                 *
 *                                                                                      */
//========================================================================================
type Scala[A, B] = A => B

given Scala: Category[Scala] with
  def id[A]: A => A = identity[A]
  extension [A, B, C]
    (g: B => C) def ◦ (f: A => B) = g compose f


// object DisciplineExamples extends App {
//   // import cats.laws.discipline.MiniInt
//   import cats.implicits.*
//   import cats.laws.discipline.DeprecatedEqInstances.catsLawsEqForFn1
//   CategoryTests[Scala].category[Int, Int, Int, Int].all.check()
// }


//======================
//  Opposite category
//======================

type Op[C[_, _]] = [A, B] =>> C[B, A]

given Op[C[_, _]](using C: Category[C]): Category[Op[C]] =
  new Category[Op[C]]:
    def id[A] = C.id[A]
    extension [A, B, C](g: C <~ B) def ◦ (f: B <~ A) = C.◦(f)(g)
