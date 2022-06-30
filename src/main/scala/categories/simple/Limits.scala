package categories.simple

import annotations1.*
import isEqual.*

// ------------
// Diagrams
// ------------


  // A diagram D in a category is a collection of objects {i : D[i] exists}
// i.e. The members of the type class D,  TC[D] = {i: Type | summon[D[i]] }
trait Diagram[D[_], ~>[_, _]: Category] {
  // together with arrows between certain objects in the diagram
  def arrows[i : D, j : D]: Set[i ~> j]
}

//trait Diagram2[~>[_, _]: Category, D] {
//  // together with arrows between certain objects in the diagram
//  def arrows[i <: D, j <: D]: Set[i ~> j]
//}
//
//object Diagram2 {
//  def pd[A, B]
//}

// Given a pair of types (C, A) produce an arrow
case class ConeArrow[C, i: D, ~>[_, _]: Category, D[_]](f: C ~> i)
  
// --------
// Cones
// --------

trait Cone[D[_], ~>[_, _]: Category] {
  // A cone for a diagram
  val D: Diagram[D, ~>]
  // consists of an object
  type C
  // together with arrows f[i]: C ~> i
  type Arrow[i] = ConeArrow[C, i, ~>, D]
  
  def f[i: D: Arrow]: C ~> i = summon[Arrow[i]].f
  
  def fp[i: D]: C ~> i

  // such that
  @Law
  def factorization[i: D: Arrow, j: D: Arrow] =
    D.arrows[i, j].map { g => g ◦ f[i] <-> f[j] }
}
 
// --------
// Limits
// --------

// A Limit for a Diagram D is a D-Cone
trait Limit[D[_], ~>[_, _]: Category] extends Cone[D, ~>] {

  // with the property that for any other D-cone c there is exactly one arrow
  def unique(c: Cone[D, ~>]): c.C ~> C

  // such that
  @Law
  def factorization[i: D](c: Cone[D, ~>])(
    using 
        c.Arrow[i], 
          Arrow[i]): IsEq[c.C ~> i] =

//    c.f[i] <-> f[i] ◦ unique(c)
  ???
}

object LimitExamples {
  def prod[A, B, ~>[_, _]: Category, C0] = {
    
    trait Prod[X]
    
    given prodA: Prod[A] = ???
    given prodB: Prod[B] = ???
    
    val d = new Diagram[Prod, ~>] {
      def arrows[i : Prod, j : Prod]: Set[i ~> j] = Set.empty
    }    
    
    d.arrows[A, B]
    
    val c = new Cone[Prod, ~>] {
      val D = d
      type C = C0
      def fp[i: Prod]: C ~> i = ???
    }
  }
}
// If an instance of `Limit` exists, it is said to have the
// universal property with respect to D-cones.


// Example 1: Given the arrow-less diagram {A, B}, a D-cone is an 
// object C together with two arrows A <~f~ C ~g~> B
// A limit of all such cones is the product of A and B:
// object Examples
//   val prod = new Limit[Scala, ]


    

// -----------
// Co-cones
// -----------


// A co-cone for a diagram
trait CoCone[~>[_, _]: Category, P[_]] {
  val D: Diagram[P, ~>]
  // consists of an object
  type C
  // together with arrows f[i]: C ~> D[i]
  def f[i : P]: i ~> C

  // such that
  @Law
  def factorization[i : P, j : P] =
    D.arrows[i, j].map { g => f[j] ◦ g <-> f[i] }
}

// A co-limit for a Diagram D
trait CoLimit[~>[_, _]: Category, P[_]] {
  // is a D-coCone c
  val c: CoCone[~>, P]

  // with the property that for any other D-cone d
  // there is exactly one arrow
  def unique(d: CoCone[~>, P]): c.C ~> d.C

  // such that
  @Law
  def factorization[i : P](d: CoCone[~>, P]): IsEq[i ~> d.C] = ???
//    d.f[i] <-> unique(d) ◦ c.f[i]
}

object ColimiExamples {
  // co-products
  trait IsCoProduct[~>[_, _]: Category, C, A] {
    def f: A ~> C
  }

  // example
  given IsCoProduct[Scala, String | Int, Int] with {
    def f: Int => String | Int = identity
  }
  
}

