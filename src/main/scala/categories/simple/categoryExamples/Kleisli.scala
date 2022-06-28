package categories.simple.categoryExamples

import cats.Monad
import categories.simple.Category

// --------- Monadic functions -----------
// A monadic function is a function A => M[B]
// This is actually a family of categories, one for each Monad[M]

case class Kleisli[M[_]: Monad, A, B](run: A => M[B]) {
  def apply(a: A) = run(a)
}

given KleisliCat[M[_]](using M: Monad[M]): Category[[A, B] =>> Kleisli[M, A, B]] with {
  import cats.implicits.*
  import scala.language.implicitConversions

  def id[A] = Kleisli(M.pure)

  extension [A, B, C] (g: Kleisli[M, B, C]) def ◦ (f: Kleisli[M, A, B]): Kleisli[M, A, C] =
    // Kleisli { a =>
    //   for {
    //     b <- f(a)
    //     c <- g(b)
    //   } yield c
    // }
  Kleisli(a => f(a) flatMap g.run)
}

object CategoryExamples {

  import cats.syntax.functor.*, cats.syntax.flatMap.*
  import scala.language.implicitConversions

  implicit val MO: Monad[Option] = ???
  summon[Category[[A, B] =>> Kleisli[Option, A, B]]]

  val f = Kleisli((x: Double) => if (x == 0) None else Some(1 / x))

  val f4 = f ◦ f ◦ f ◦ f

  def associativity[M[_]: Monad, A, B, C, D](
    f: Kleisli[M, A, B],
    g: Kleisli[M, B, C],
    h: Kleisli[M, C, D],
  )(using
    CanEqual[Kleisli[M, A, D], Kleisli[M, A, D]]
  ) = {
    h ◦ (g ◦ f) == h ◦ Kleisli { (a: A) =>
                        for {
                          b <- f(a)
                          c <- g(b)
                        } yield c
                      }

    h ◦ (g ◦ f) == Kleisli { (a: A) =>
                    for {
                      c1 <- for {
                          b <- f(a)
                          c <- g(b)
                        } yield c
                      d <- h(c1)
                    } yield d
                  }
    // while
    (h ◦ g) ◦ f == Kleisli { (b: B) =>
                    for {
                      c <- g(b)
                      d <- h(c)
                    } yield d
                  } ◦ f

    (h ◦ g) ◦ f == Kleisli { (a: A) =>
                    for {
                      b <- f(a)
                      d <-
                        for {
                          c  <- g(b)
                          d0 <- h(c)
                        } yield d0
                    } yield d
                  }

  }

  def identityLeft[M[_], A, B](f: Kleisli[M, A, B])(using M: Monad[M], eq: CanEqual[Kleisli[M, A, B], Kleisli[M, A, B]]) = {
    // (f ◦ id[A]) === f
    Kleisli { (a: A) =>
      for {
        b <- f(a)
        c <- M.pure(b)
      } yield c
    } == f
    // or
    Kleisli { (a: A) => f(a) flatMap M.pure } == f
  }

  // def identityRight[A, B](f: Hom[A, B])(using Eq[Hom[A, B]]) =
  //   (id[B] ◦ f) === f


  // summon[Monad[util.Try]]

  // type EitherMonad[R] =  util.Either[Int, R]
  // summon[Category[[A, B] =>> A => EitherMonad[B]]]

}

