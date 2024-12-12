package categories.simple.categoryExamples

import categories.simple.CategoryS

import scala.annotation.targetName

trait MatrixDomain:
  type eltype
  val dim: Int

class Matrix[T, m <: Int, n <: Int]

type MatrixT[T] = [m <: Int, n <: Int] =>> Matrix[T, m, n]

def matrixCat[T]: CategoryS[Int, MatrixT[T]] =
  new CategoryS:
    def id[A <: Int] = ???
    extension [A <: Int, B <: Int, C <: Int] (g: B ~> C)
      def ◦ (f: A ~> B): A ~> C = ???
