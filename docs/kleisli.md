### Example: Kleisli categories

Most categorical concepts defined in [Cats](https://typelevel.org/cats/typeclasses.html) (such as the [Monad](https://github.com/typelevel/cats/blob/master/core/src/main/scala/cats/Monad.scala) trait) are specialized to operate on a single category: **Scala**.  We'll use them occasionally and will refer to them by using the fully qualified name: `cats.Monad`, `cats.Functor`, etc.



Given a `cats.Monad[M]` instance over a type function `M[_]` we can construct a `Category[M]` whose objects are *all* the types and arrows are monadic functions (i.e. functions of the form `A => M[B]`).

This is known as the **Kleisli** category *[associated](https://en.wikipedia.org/wiki/Kleisli_category) with* the monad `M` (the linked article uses a more general version than ours).

In order to avoid any ambiguity when using composition, we''ll create a case class to wrap our monadic functions:

```scala
import cats.Monad
import cats.implicits.*
import scala.language.implicitConversions

case class Kleisli[M[_]: Monad, A, B](run: A => M[B])
  def apply(a: A) = run(a)

given KleisliCat[M[_]](given M: Monad[M]): Category[[A, B] =>> Kleisli[M, A, B]]

  def id[A] = Kleisli(M.pure)
  
  def [A, B, C] (g: Kleisli[M, B, C]) ◦ (f: Kleisli[M, A, B]): Kleisli[M, A, C] =
    Kleisli { a => 
      for { 
        b <- f(a) 
        c <- g(b)
      } yield c
    }
  // or:
  // Kleisli(a => f(a) flatMap g.run)
```

>  (Dotty feature: [type lambas](https://dotty.epfl.ch/docs/reference/new-types/type-lambdas.html))

> The argument of `Category` is a two-argument anonymous type function:
>
> ​	 `[A, B] =>> Kleisli[M, A, B]`. 
>
> (At this point the type function `M` is already fixed, since it is an argument of `Kleisli`). 
>
> For example, if `M == List` then for any two types `A, B` the anonymous function will evaluate to the type `Kleisli[List, A, B]`  (which is just a wrapper for functions `A => List[B]`)

Example:

```scala
import cats.implicits.*

val f = Kleisli((x: Double) => if (x == 0) None else Some(1 / x))
val f4 = f ◦ f ◦ f ◦ f
```

What this tells us is that the ubiquous `flatMap` operation is simple arrow composition in a category where arrows are monadic functions (the Kleisli category).



It is always interesting to translate categorical definitions in terms of concrete category operations. For example the composition law:

<table border="0">
<tbody>
<tr>
<td align="center"><pre>h ◦ (g ◦ f)</pre></td>
<td align="center"><pre>===</pre></td>
<td align="center"><pre>(h ◦ g) ◦ f</pre></td>
</tr>
<tr>
<td colspan=3 align="center">becomes</td>
</tr>
<tr>
   <td>
   <pre>
Kleisli { (a: A) =>
  for {
    c <- for {
        b  <- f(a)
        c0 <- g(b)
      } yield c0
    d <- h(c)
  } yield d
}</pre>
</td>
<td align="center"><pre>===</pre></td>
 <td>
     <pre>Kleisli { (a: A) =>
  for {
    b <- f(a)
    d <-
      for {
        c  <- g(b)
        d0 <- h(c)
      } yield d0
  } yield d
}                  
</pre>
  </td>
</tr>
</tbody>
</table>  

which intuitively looks like it should hold for any sane implementation of `flatMap` .

## 
