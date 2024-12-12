package algebra

trait Semiring[R]:
  def zero: R
  def one: R
  def closure: R => R

  extension (x: R)
    def + (y: R) : R
    def * (y: R) : R


given sb: Semiring[Boolean]:
  val zero = false
  val one = true
  val closure = _ => true

  extension (x: Boolean)
    def + (y: Boolean): Boolean = x || y
    def * (y: Boolean): Boolean = x && y


enum Matrix[A]:
  case Scalar(a: A)
  case Matrix(aa: Array[Array[A]])

object Matrix:
  extension [A] (x: Matrix[A])
    def + (y: Matrix[A]): Matrix[A] = ???
    def * (y: Matrix[A]): Matrix[A] = ???
end Matrix

type BlockMatrix[A] =
  (
    Matrix[A], Matrix[A],
    Matrix[A], Matrix[A]
  )

