package categories.simple

import categories.simple.functor.*
import cats.implicits.*
import discipline1.*


def yonedaLemma[C[_, _], G[_], X0](G: (C --> Scala)[G])
  (using C: Category[C] ) =
  import C.id

  val F: (C --> Scala)[C[X0, _]] = homFunctor[C, X0]

  // at this point we know that F, G are functors and C is a category,
  // so we can simplify a bit by working with the natural transformation
  // as a polymorhic function instead of the wrapper type Nat:
  type ==>[F[_], G[_]] = [X] => F[X] => G[X]

  // ----------------
  // The Yoneda lemma
  // ----------------

  // the canonical mapping:
  val y : (C[X0, _] ==> G) => G[X0] =
                     φ     => φ[X0]( id[X0] )

  // is a bijection with inverse:
  val yp: G[X0] => (C[X0, _] ==> G) =
            z   =>   ([X]    => (f: C[X0, X]) => G(f)(z))


  // Or more concisely:
  val yoneda: (C[X0, _] ==> G) ≅ G[X0] =
    Bijection(y, yp)

  // i.e.
  // y  ◦ yp <-> identity[G[X0]]
  // yp ◦ y  <-> identity[CX0 ==> G]

  // Proof:
  // a) yp(y(𝜑)) == 𝜑
  def proofPart1(φ: C[X0, _] ==> G) =

    List(
      // by definition of y:
      yp(y(φ)) <-> yp( φ[X0]( id[X0] ) ),

      // by definition of yp:
      yp(y(φ)) <-> ( [X] => (f: C[X0, X]) => G(f) { φ[X0] { id[X0] } } ),

      // which in Scala is the same as:
      yp(y(φ)) <-> ( [X] => (f: C[X0, X]) => (G(f) ◦ φ[X0]) { id[X0] } ),

      // by the naturality condition: G(f) ◦ φ[X0] === φ[X] ◦ F(f)
      yp(y(φ)) <-> ( [X] => (f: C[X0, X]) => (φ[X] ◦ F(f)) { id[X0] } ),

      yp(y(φ)) <-> ( [X] => (f: C[X0, X]) => φ[X] { F(f) { id[X0] } } ),

      // by definition of homFunctor: F(f)( id[X0] ) == f ◦ id[X0]
      // and by definition of composition: f ◦ id[X0] == f
      yp(y(φ)) <-> ( [X] => (f: C[X0, X]) => φ[X] { f } ),

      // applying eta reduction two times:
      // yp(y(φ)) == ([X] => φ[X]) = φ
      yp(y(φ)) <-> φ

    )



  // b) y(yp(z)) == z
  def proofPart2(z: G[X0]) =

    List(
      // by definition of yp:
      y(yp(z)) <-> y( [X] => (f: C[X0, X]) => G(f)(z)),

      // by definition of y:
      y(yp(z)) <-> ([X] => (f: C[X0, X]) => G(f)(z)) { id[X0] },

      // evaluating the right hand side
      y(yp(z)) <-> G(id[X0])(z),

      // Since G maps identities to identities:
      // G(id[X0]) == Scala.id[G[X0]] == identity[G[X0]] = x => x
      y(yp(z)) <-> z
    )

    // Q.E.D

