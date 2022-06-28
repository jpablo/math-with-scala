package categories.simple.categoryExamples
import categories.simple.*

type FinSet[A, B] = Set[A] => Set[B]

given FinSet: Category[FinSet] with
  def id[A]: Set[A] => Set[A] = identity[Set[A]]
  extension [A, B, C] (g: Set[B] => Set[C]) def ◦ (f: Set[A] => Set[B]) = g compose f

// more generally:
type TypeFamily[F[_]] = [A, B] =>> F[A] => F[B]

given [F[_]]: Category[TypeFamily[F]] with
  def id[A]: F[A] => F[A] = identity[F[A]]
  extension [A, B, C] (g: F[B] => F[C]) def ◦ (f: F[A] => F[B]) = g compose f
