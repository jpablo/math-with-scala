package categories.simple.functor

import categories.simple.*

// [A] =>> List[A]
trait FreeMonoid
  [S[_, _], T[_, _]]
  (using S: Category[S], T: Category[T])
extends (S --> T)[List]:
  def map[A, B](f: S[A, B]): T[List[A], List[B]] = ???


