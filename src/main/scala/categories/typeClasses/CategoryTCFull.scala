package categories.typeClasses

import annotations1.Law

trait HomId[A, ~>[_, _]]:
  val id: A ~> A

object HomId:
  def apply[A, ~>[_, _]](using ev: HomId[A, ~>]) = ev.id


trait HomCompose[A, B, C, ~>[_, _]]:
  def compose(g: B ~> C, f: A ~> B): A ~> C

trait HomAndThen[A, B, C, ~>[_, _]]:
  extension (f: A ~> B) def >>> (g: B ~> C): A ~> C

/**
 * A Category where the operations are delegated to type classes
 * This gives maximum flexibilty but it is very verbose
 */
trait Category[~>[_, _]]:
  type Id[A] = HomId[A, ~>]
  type AndThen[A, B, C] = HomAndThen[A, B, C, ~>]

  def id[A: Id]: A ~> A = summon[Id[A]].id

  extension [A, B, C] (f: A ~> B) def >>> (g: B ~> C)
    (using AndThen[A, B, C]): A ~> C = f >>> g

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
      AndThen[A, C, D],
      AndThen[A, B, C],
      AndThen[B, C, D],
      AndThen[A, B, D]
  ) =
      ((f >>> g) >>> h) == (f >>> (g >>> h))

end Category
