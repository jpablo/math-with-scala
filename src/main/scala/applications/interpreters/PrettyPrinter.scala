package applications.interpreters

import categories.simple.*
import categories.simple.monoidalCategories.{BraidedMonoidalCategory, MonoidalCategory, StrictMonoidal, CartesianMonoidalCategory}
import categories.simple.{_, given}
import categories.simple.categoryExamples.{_, given}
import categories.simple.functor.*
import categories.simple.functor.Functor.Id

case class PP[A, B](value: String)

given PPCat: Category[PP] with
  def id[A] = PP("id")
  extension [A, B, C] (g: PP[B, C]) def ◦ (f: PP[A, B]) = PP(s"(${g.value} ◦ ${f.value})")

object PPMonCatProduct extends CartesianMonoidalCategory[PP, Tuple2] with MonoidalCategory[PP] with InternalProduct[PP]:
  def fst[A, B]: PP[(A, B), A] = PP("fst")
  def snd[A, B] = PP("snd")
  extension [A, B, X] (f: PP[X, A]) def * (g: PP[X, B]): PP[X, (A, B)] = PP(s"(${f.value} * ${g.value})")
  type T = EmptyTuple
  val terminal =
    new Terminal:
      def arrow[Y]: PP[Y, EmptyTuple] = PP("!")

  def tensor = ???

  type PP3[A, B] = Prod3[PP, PP, PP][A, B]

  val associator =
    new NaturalIsomorphism[Lassoc, Rassoc, PP3, PP] with (Lassoc ==> Rassoc)[PP3, PP]:
      val source: (PP3 --> PP)[Lassoc] = new:
        def map[A, B](f: PP3[A, B]): PP[Lassoc[A], Lassoc[B]] = PP("Lassoc.map")

      val target: (PP3 --> PP)[Rassoc] = new:
        def map[A, B](f: PP3[A, B]): PP[Rassoc[A], Rassoc[B]] = PP("Rassoc.map")

      def apply[A] = PP("a")
      def inverse[A] = PP("aI")

  def leftUnitor =
    new NaturalIsomorphism with (L ==> Id)[PP, PP]:
      val source: (PP --> PP)[L] = new:
          def map[A, B](f: PP[A, B]): PP[L[A], L[B]] = ???

      val target: (PP --> PP)[Id] = Functor.identity[PP]
      def apply[A] = ???
      def inverse[A] = ???

  def rightUnitor = ???


object Examples extends App:
  import isEqual.*
  def pprint[A, B](e: IsEq[PP[A, B]]) =
    s"${e.lhs.value} <-> ${e.rhs.value}"
  // associativity
  trait A; trait B; trait C; trait D;
  val f = PP[A, B]("f")
  val g = PP[B, C]("g")
  val h = PP[C, D]("h")
  println(pprint(PPCat.associativity(f, g, h)))
  println(pprint(PPCat.identityL(f)))
  println(pprint(PPCat.identityR(f)))
  // -----------------
  import PPMonCatProduct.*
  val j = Δ >>> (id ** Δ) >>> aI
  println(j.value)
