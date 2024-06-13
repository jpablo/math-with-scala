# Algebraic structures

Per [wikipedia](https://en.wikipedia.org/wiki/Algebraic_structure):

> An **algebraic structure** on a [set](https://en.wikipedia.org/wiki/Set_(mathematics)) *A* (called the **underlying set**, **carrier set** or **domain**) is a collection of [operations](https://en.wikipedia.org/wiki/Operation_(mathematics)) on *A* of finite [arity](https://en.wikipedia.org/wiki/Arity), together with a finite set of [identities](https://en.wikipedia.org/wiki/Identity_(mathematics)), called [axioms](https://en.wikipedia.org/wiki/Axiom#Non-logical_axioms) of the structure that these operations must satisfy.

In programming terms, an algebraic structure is an [abstract data type](https://en.wikipedia.org/wiki/Abstract_data_type) `A` with an accompaning library of functions that operate on it and with a test suite that captures carefully chosen properties.

The key characteristic of such definition is that it captures an abstract notion that is (presumably) satisfied by more than one concrete data structure. 

Take for example the pervasive [Monoid](https://en.wikipedia.org/wiki/Monoid):

> Suppose that *S* is a [set](https://en.wikipedia.org/wiki/Set_(mathematics)) and • is some [binary operation](https://en.wikipedia.org/wiki/Binary_operation) *S* × *S* → *S*, then *S* with • is a **monoid** if it satisfies the following two axioms:
>
> - Associativity
>
> For all *a*, *b* and *c* in *S*, the equation (*a* • *b*) • *c* = *a* • (*b* • *c*) holds.
>
> - Identity element
>
> There exists an element *e* in *S* such that for every element *a* in *S*, the equations *e* • *a* = *a* • *e* = *a* hold.

In traditional object oriented fashion abstractions like this are expressed via inheritance.  This presents several challenges, the most serious one being that we cannot retroactively make an existing type extend our new hyphotetical Monoid trait.

For this reason the common way to encode algebraic structures is via type classes:

```scala
trait Monoid[S] {
    def id: S
    def (x: S) ∗ (y: S): S
}

class MonoidAxioms[S: Arbitrary: Monoid] extends Properties("Monoid") {
    val M = summon[Monoid[S]]
  	val id = M.id

    property("associativity") = forAll { (a: S, b: S, c: S) => 
        ((a * b) * c) == (a * (b * c))
    }

    property("left identity") = forAll { (a: S) => 
        id * a == a
    }

    property("right identity") = forAll { (a: S) => 
        a * id == a
    }
}
```

As you can see this definition mirrors nicely the corresponding math definition

| Math                                                         | Scala                                                        |
| ------------------------------------------------------------ | ------------------------------------------------------------ |
| A set $S$                                                    | The set of values of type `S`                                |
| A binary operation $\cdot \: S \times S\to S$                | A binary function `(x: S) ∗ (y: S): S`                       |
| Associativity axiom                                          |                                                              |
| For all *a*, *b* and *c* in *S*, the equation (*a* • *b*) • *c* = *a* • (*b* • *c*) holds. | `forAll { (a: S, b: S, c: S) => ((a * b) * c) == (a * (b * c)) }` |
| Identity                                                     |                                                              |
| There exists an element *e* in *S* such that for every element *a* in *S*, the equations *e* • *a* = *a* • *e* = *a* hold. | `def id: S`<br />`forAll((a: S) =>  id * a == a)`<br />`forAll((a: S) =>  a * id == a)` |



Let's compare it with a classic interface such as Java's `Iterator`:

```java
public interface Iterator<A> {
  boolean hasNext();  
  A next();
  ...
}
```

or Scala's `GenTraversableOnce`:

```scala
trait GenTraversableOnce[+A] extends Any {
  def foreach[U](f: A => U): Unit
  def size: Int
  def isEmpty: Boolean
  ...
}
```



A perspective that I find useful is to consider algebraic structures as *polymorphic* data structures with accompanying library functions that satisfy a carefully chosen test suite.

The polymorphic bit is the important part: this is what allows mathematical objects to model so many different phenomena.
