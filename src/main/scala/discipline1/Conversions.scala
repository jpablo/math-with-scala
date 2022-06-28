package discipline1

import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.util.Pretty
//import cats.Eq
import scala.language.implicitConversions
import scala.annotation.targetName
import scala.util.NotGiven

final case class IsEq[A](lhs: A, rhs: A) //(using CanEqual[A, A]) //(using ev1: NotGiven[A =:= Any])

type Eq[A] = CanEqual[A, A]

extension [A] //(using Eq[A])
  (lhs: A) def <-> (rhs: A) = IsEq(lhs, rhs)

//extension [A, B, F[_]] (lhs: F[A])
//  @targetName("IsEqF")
//  def <-> (rhs: F[B])(using ev: F[A] =:= F[B]) = IsEq(lhs, rhs)

//implicit def catsLawsIsEqToProp[A](isEq: IsEq[A])(implicit ev: Eq[A], pp: A => Pretty): Prop =
//  isEq match {
//    case IsEq(x, y) =>
//      if (ev.eqv(x, y)) Prop.proved
//      else
//        Prop.falsified :| {
//          val exp = Pretty.pretty[A](y, Pretty.Params(0))
//          val act = Pretty.pretty[A](x, Pretty.Params(0))
//          s"Expected: $exp\n" + s"Received: $act"
//        }
//  }

