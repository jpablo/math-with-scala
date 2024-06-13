## Pattern matching on types

[show matching at different levels]

## Different kinds of Type Classes



### The simplest: dummy values

```scala
// https://github.com/ncreep/compile-time-logic-programming-talk.git

sealed trait HList
case class ::[H, T <: HList](head: H, tail: T) extends HList { ... }

sealed trait Member[Elem, L <: HList]

// Base case:
//  H is a member of H :: T
// i.e.
//  (H, H :: T) \in R
implicit def headMember[H, T <: HList] = new Member[H, H :: T] {}

// Inductive step:
// (E, T) \in R <=> (E, H :: T) \in R
implicit def memberAnywhere[Elem, H, T <: HList]
  (implicit
   m: Member[Elem,      T]
  ) : Member[Elem, H :: T] = new Member[Elem, H :: T] {}
}
```

In this example types are matched using two type functions (`Member[E, ::[A, B]]`)



### Information attached to types

```scala
// https://gist.github.com/aaronlevin/d3911ba50d8f5253c85d2c726c63947b#file-events-scala-L67

trait Named[E] {
	val name: String
}

implicit val baseCaseNamed = new Named[EndOfList] {
  val name: String = ""
}

// Named induction step: (E, Tail)
implicit def inductionStepNamed[E,Tail](
	implicit
            n: Named[ E      ],
    tailNames: Named[   Tail ]
) =        new Named[(E,Tail)] {
  val name: String = s"${n.name}, ${tailNames.name}"
}
```

Constructing summoned values requires running some computation.



### Operations attached to types

```scala
trait Monoid[A] {
  def empty: A
  def combine(a1: A, a2: A): A
}

// A monoid over an arbitrary F[_] cannot be constructed because we don't have any information on how to access elements inside F
```

