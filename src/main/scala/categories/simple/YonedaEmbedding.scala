package categories.simple

import categories.simple.functor.*
import isEqual.*


def yonedaEmbedding[X, Y, C[_, _]](using C: Category[C]):
  C[X, Y] ≅ (C[_, X] ==> C[_, Y])[ Op[C], Scala ] = ???

//    def arrowToNat(z: C[X, Y]) =
//      new Nat {
//        val from = presheaf[C, X]
//        val to = presheaf[C, Y]
//        def apply[A] = f => z ◦ f
//      }
//
//    Bijection(arrowToNat, φ => φ[X](C.id))
