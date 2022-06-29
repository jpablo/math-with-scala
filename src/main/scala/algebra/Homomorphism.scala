package algebra

case class Homomorphism[P[_], A: P, B: P](f: A => B) extends (A => B):
  def apply(a: A) = f(a)
