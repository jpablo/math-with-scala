package categories.simple

import categories.simple.functor.*
import Functor.Id

trait Adjunction
  [C[_,_], D[_,_], F[_], G[_]]
  (
    val f: (C --> D)[F],
    val g: (D --> C)[G],
  )
  (using c: Category[C], d: Category[D]):
  
    import c.~>; import d.~> as ->
    
    // A: C, B: D
    def iso[A, B]:
      (F[A] -> B) ≅ (A ~> G[B])
    
    // η : 1A ==> GF
    def unit: (Id ==> G ⊙ F)[C, C] =
      new Nat:
        val source = Functor.identity[C]
        val target = g ⊙ f
        def apply[X] = iso.from(d.id[F[X]])
      
    // ε : FG ==> 1B
    def counit: (F ⊙ G ==> Id)[D, D] = ???
  
end Adjunction

/**
 * (F ⊣ G)[C, D]
 */
type ⊣[F[_], G[_]] =
  [C[_, _], D[_, _]] =>> Adjunction[C, D, F, G]
