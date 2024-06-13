# Fixpoint

A fixed point of a function $f: A \rightarrow B$ is a value $a\in A$ such that $f(a) = a$.

In the same spirit, a fixed point of a type function `F[_]` is a type `A` such that `F[A] =:= A`.

The Scala compiler won't accept this definition though, except for the trivial case where `F` is the identity.

For an arbitrary `F` (such as `List`), there's no way to make this work. The next best next thing would be to find a type `A'` that is isomorphic to `A`  (i.e. $\mathtt{A}\simeq\mathtt{A'}$).

Consider the following definition:

```scala
case class Fix[F[_]](unfix: F[Fix[F]])
```

For a given `F[_]`,  we have two functions:

The normal `apply` method defined for case classes
$$
\begin{matrix}
\mathtt{Fix.apply}: &  \mathtt{F[Fix[F]]} & \rightarrow &\mathtt{Fix[F]} \\
& fa & \mapsto & \mathtt{Fix}(fa)
\end{matrix}
$$
and it's inverse (a getter)
$$
\begin{matrix}
\mathtt{\_.unfix}: &  \mathtt{Fix[F]} & \rightarrow &\mathtt{F[Fix[F]]} \\
& \mathtt{Fix}(fa) & \mapsto & fa
\end{matrix}
$$
This means that
$$
\mathtt{F[Fix[F]]} \simeq \mathtt{Fix[F]}
$$
and thus we can say that the type `Fix[F]` is a fixed point (almost!) of the type function `F[_]`.

`Fix` can be used to wrap each `Toy` value, like so:

```scala
// let's create an alias to simplify things
type F[A] = Toy[Int, A]

val f0: F[Fix[F]] =                                          Done(): F[Nothing]
val f1:   Fix[F]  =                                   Fix[F](Done())
val f2: F[Fix[F]] =                         Output(1, Fix[F](Done()))
val f3:   Fix[F]  =                  Fix[F](Output(1, Fix[F](Done())))
val f4: F[Fix[F]] =        Output(2, Fix[F](Output(1, Fix[F](Done()))))
val f5:   Fix[F]  = Fix[F](Output(2, Fix[F](Output(1, Fix[F](Done())))))
```



Let's first focus only on the types involved :

![](diagrams/Fixpoint2.png)