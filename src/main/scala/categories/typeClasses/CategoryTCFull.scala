package categories.typeClasses

import annotations1.Law

trait HomId[A, ~>[_, _]]:
  val id: A ~> A

object HomId:
  def apply[A, ~>[_, _]](using ev: HomId[A, ~>]) = ev.id


trait HomAndThen[A, B, C, ~>[_, _]]:
  def andThen (f: A ~> B, g: B ~> C) : A ~> C
  def compose (g: B ~> C, f: A ~> B) : A ~> C = andThen(f, g)
  extension (f: A ~> B)
    def >>> (g: B ~> C) : A ~> C = andThen(f, g)

/**
 * A Category where the operations are delegated to type classes
 * This gives maximum flexibilty but it is very verbose
 */
trait Category[~>[_, _]]:
  type Id[A] = HomId[A, ~>]
  type AndThen[A, B, C] = HomAndThen[A, B, C, ~>]

  def id[A: Id]: A ~> A = summon[Id[A]].id

  @Law
  def identityRight[A, B: Id](f: A ~> B) (using AndThen[A, B, B]) =
      (f >>> id[B]) == f

  @Law
  def associativity[A, B, C, D](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  )(
    using
      a1: AndThen[A, B, C],
      a2: AndThen[A, B, D],
      a3: AndThen[A, C, D],
      a4: AndThen[B, C, D],
  ) =
      a3.andThen(a1.andThen(f, g), h) == a2.andThen(f, a4.andThen(g, h))

end Category
