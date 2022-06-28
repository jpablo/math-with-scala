package categories.highOrder

import categories.simple.{_, given}
import categories.simple.functor.*

/**
 * A Category where objects are type functions F[_] with a type class instance P[F] available
 * @tparam P A type class that objects must belong to
 * @tparam Hom
 */
trait CategoryF[P[f[_]], Hom[f[_], g[_]]]:

  type ~>[F[_], G[_]] = Hom[F, G]

  def id[F[_]: P]: F ~> F

  extension [A[_]: P, B[_]: P, C[_]: P]
    (f: A ~> B) def >>> (g: B ~> C): A ~> C

  extension [F[_]: P, G[_]: P, H[_]: P]
    (m: G ~> H) def ◦ (n: F ~> G): F ~> H = n >>> m

type IdF[A[_]] = Any

// Simpler version without any type class constraint

trait CategoryF1[Hom[f[_], g[_]]] extends CategoryF[IdF, Hom]:
  def id[F[_]]: F ~> F
  extension[A[_], B[_], C[_]] (f: A ~> B) def andThen (g: B ~> C): A ~> C
  // implement CategoryF methods in terms of CategoryF1 methods
  def id[F[_] : IdF]: F ~> F = id[F]
  // Is it possible to use the same operator (>>>) in both traits?
  extension[A[_] : IdF, B[_] : IdF, C[_] : IdF] (f: A ~> B) def >>> (g: B ~> C): A ~> C = f andThen g

// --------------------
// Category of functors
// --------------------

// objects - Functors F: C -> D
// morphisms - Nat(F, G) with vertical composition
// also denoted as [C, D]

def FunctorCat[C[_, _]: Category, D[_, _]: Category] =
  // objects are functors F: C --> D
  type Func[F[_]] = (C --> D)[F]
  // morphisms are natural transformations n: F ==> G between functors F and G
  type ==>[F[_], G[_]] = Nat[F, G, C, D]

  new CategoryF[Func, ==>] {

    // identity: natural identity at F[_]
    def id[F[_]](using F: Func[F]) = Nat.identity(F)

    // composition is vertical composition of nat transfs.
    extension[F[_]: Func, G[_]: Func, H[_]: Func]
      (φ: F ==> G) def >>> (ψ : G ==> H)  = ψ * φ
  }
end FunctorCat

// ----------------------
// Category of presheaves
// ----------------------

// The objects of this category are presheaves of C on S:
// Presheaf[C, S][F] =:= (Op[S] --> C) [F]

def Psh[S[_, _], C[_, _]]
  (using S: Category[S], C: Category[C]) =
  FunctorCat(using Op[S], C)

// we're interested in the category
// Psh(S, Scala) of presheaves of types on S
//
// The category S is canonically embedded in the latter, by the Yoneda
// embedding:

def yonedaEmbedding[S[_, _]: Category, i] =
  homFunctor[Op[S], i](using Op[S])


// ----------------------
// Category of categories
// ----------------------

// Definition 1.5.1. The category Cat has
// • As objects, small categories;
// • As morphisms, the functors between them.

trait CategoryTC2[P[c[_, _]], HomCat[c[_, _], d[_, _]]] {
  type ~>[C[_, _], D[_, _]] = HomCat[C, D]

  def id[F[_, _]](using P[F]): F ~> F

  extension [F[_, _]: P, G[_, _]: P, H[_, _]: P]
    (m: G ~> H) def ◦ (n: F ~> G): F ~> H
}

type --->[C[_,_], D[_,_]] = Functor[?, C, D]

/**
 * The category of small categories:
 * Objects are categories and arrows are functors
 */
object CatCat extends CategoryTC2[Category, --->] {

  def id[C[_,_]: Category]: C ---> C = Functor.identity[C]

  extension [F[_,_]: Category, G[_,_]: Category, H[_,_]: Category]
    (m: G ---> H) def ◦ (n: F ---> G): F ---> H =
    m ⊙ n
}
