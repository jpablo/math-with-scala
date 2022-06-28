package categories

object Kinds {

  // regular functor
  // examples: List[A], Option[A]
  // T -> T
  type T2T = [A] =>> Any

  // type TT2T = [_, _] =>> Any
  // aka: Hom[_, _]

  // functors to types
  // cats.Functor[F[_]], cats.Monad[F[_]]
  // (T -> T) -> T
  type F2T = [F[_]] =>> Any

  // functors to functors
  // (T -> T) -> (T -> T)
  type F2F = [F[_]] =>> ([A] =>> Any)

  // product of functors to types
  // (T -> T, T -> T) -> T
  type Hom2 = [F[_], G[_]] =>> Any
  // aka FF2T
}