package sets

import discipline1.*
import annotations1.*

object Orders:

  // A Preorder is a set A
  trait Preorder[A]:
    // with a binary relation
    extension (a: A) def < (b: A): Boolean

    // that is reflexive and transitive
    @Law
    def reflexivity(a: A) =
      a < a

    @Law
    def transitivity(a: A, b: A, c: A) =
      if (a < b && b < c)
        a < c
      else
        true
  end Preorder

  // also called a Partial Order (poset)
  trait Order[A] extends Preorder[A]:
    extension (a: A) def <= (b: A) = a < b

    @Law
    def antiSymmetry(a: A, b: A) =
      if (a <= b && b <= a)
        a == b
      else
        true


  given Order[Int] with
    extension (a: Int) def < (b: Int): Boolean = a <= b


  trait Lattice[A] extends Order[A]:
    // join
    extension (x: A) def ∨ (y: A): A = sup(x, y)
    // meet
    extension (x: A) def ∧ (y: A): A = inf(x, y)

    def least: A
    def greatest: A

    def sup(x: A, y: A): A
    def inf(x: A, y: A): A


    @Law
    def leastLaw(x: A) = least < x

    @Law
    def greatestLaw(x: A) = x < greatest

    @Law
    def supLaw(x: A, y: A, z: A) =
      def prop(x: A, y: A, z: A) = x < z && y < z
      val s = sup(x, y)
      prop(x, y, s) && (if prop(x, y, z) then s < z else true)

    @Law
    def infLaw(x: A, y: A, z: A) =
      def prop(x: A, y: A, z: A) = z < x && z < y
      val i = inf(x, y)
      prop(x, y, i) && (if prop(x, y, z) then z < i  else true)
  end Lattice
end Orders

