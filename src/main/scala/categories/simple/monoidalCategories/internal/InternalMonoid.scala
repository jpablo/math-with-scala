package categories.simple.monoidalCategories.internal

import algebra.Monoid
import annotations1.Law
import categories.simple.Scala
import isEqual.{<->, IsEq}
import categories.simple.monoidalCategories.StrictMonoidal

trait InternalMonoid[C[_, _]](using val C: StrictMonoidal[C]):
  import C.*
  export C.*

  type M
  val m: M ⨂ M ~> M
  val e: I ~> M

  @Law
  def unitarity1 = ???
//    m ◦ (e ⨂ id[M]) <-> id[M]

  @Law
  def unitarity2 = ???
//    m ◦ (id[M] ⨂ e) <-> id[M]

  @Law
  def associativity =
    val assoc: ((M ⨂ M) ⨂ M) =:= (M ⨂ (M ⨂ M)) =
      C.associativity[M, M, M]

    val l = m ◦ (m ⨂ id[M]): ((M ⨂ M) ⨂ M ~> M)
    val r = m ◦ (id[M] ⨂ m): (M ⨂ (M ⨂ M) ~> M)
//    l <-> r

object Examples:

//  def scalaInternalMonoid[A](using monoid: Monoid[A], sm: StrictMonoidal[Scala]) =
//    new InternalMonoid[Scala]:
//      type M = A
//      val m: M ⨂ M ~> M = ???
//      val e: I ~> M = ???

  def internalMonoid[M0](using C: StrictMonoidal[Scala]) = {
    import C.*
    new InternalMonoid[Scala]:
      type M = M0
      val m: M ⨂ M => M = ???
      val e: I => M = ???
  }


  def internalMonoidToMonoid(im: InternalMonoid[Scala]) = {
    type A = im.M
    new Monoid[A]:
      val id = im.e(???)
      extension (a: A) def * (b: A): A = ???
  }
