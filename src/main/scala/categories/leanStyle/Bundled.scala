package categories.leanStyle

trait Bundled[C[_]]:
  type A
  val str: C[A]




object Bundled:
  def of[C[_], A0](using str0: C[A0]): Bundled[C] =
    new Bundled[C] {
      type A = A0
      val str: C[A] = str0
    }

