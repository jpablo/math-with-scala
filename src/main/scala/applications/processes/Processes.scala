package applications.processes

import sttp.model.Uri
import org.typelevel.discipline.Laws
import discipline1.{<->, IsEq}
import annotations1.*
import io.circe.Decoder
import sttp.client3.BodySerializer

import scala.annotation.alpha

trait ProcessDSL[~>[_, _]]:
  def httpCall[A: BodySerializer, B: Decoder](name: String, uri: Uri): A ~> B

trait ProcessDSLOps[Process[_, _]] {
  // ------------------
  // Category structure
  // ------------------
  type ~>[A, B] = Process[A, B]
  type <~[B, A] = Process[A, B]

  def id[A: TypeName]: A ~> A

  // @alpha("andThen")
  extension [A, B, C]
    (f: A ~> B) def >>> (g: B ~> C): A ~> C

  // @alpha("compose")
  extension [A, B, C]
    (g: B ~> C) def ◦ (f: A ~> B): A ~> C = f >>> g

  // Both then internal product and tensor will be Tuple2

  // ----------------
  // Internal product
  // ----------------
  def fst[A: TypeName, B: TypeName]: (A, B) ~> A
  def snd[A: TypeName, B: TypeName]: (A, B) ~> B

  // @alpha("mergeInput")
  extension [A: TypeName, B: TypeName, C: TypeName]
    (f: A ~> B) def &&& (g: A ~> C): A ~> (B, C)

  // @alpha("duplicate")
  def ∆[A: TypeName]: A ~> (A, A) =
    id[A] &&& id[A]

  // EmptyTuple will be the terminal object and monoidal unit
  type I = EmptyTuple

  def discard[A: TypeName]: A ~> I

  // ------------------
  // Monoidal structure
  // ------------------
  def assocR[X: TypeName, Y: TypeName, Z: TypeName] : ((X, Y), Z) ~> (X, (Y, Z))
  def assocL[X: TypeName, Y: TypeName, Z: TypeName] : ((X, Y), Z) <~ (X, (Y, Z))

  def injR[X]: X ~> (I, X)
  def injL[X]: X ~> (X, I)

  // tensor on objects:   A ⨂ B = (A, B)
  // tensor on morphisms: f ⨂ g = f ++ g
  // @alpha("combine")
  extension [A1, A2, B1, B2]
    (f: A1 ~> B1) def ++ (g: A2 ~> B2): (A1, A2) ~> (B1, B2)

  // ----------------------------
  // Symmetric Monoidal structure
  // ----------------------------
  def swap[X: TypeName, Y: TypeName]      : (X, Y) ~> (Y, X)
  def swapInverse[X, Y]: (X, Y) <~ (Y, X)
  // --------------
  // Category Laws
  // --------------
  @Law
  def associativity[A, B, C, D](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    h ◦ (g ◦ f) <-> (h ◦ g) ◦ f

  @Law
  def identityL[A: TypeName, B: TypeName](f: A ~> B) = (id[B] ◦ f) <-> f

  @Law
  def identityR[A: TypeName, B: TypeName](f: A ~> B) = f ◦ id[A] <-> f

  // ---------------------
  // Internal product laws
  // ---------------------
//  @Law
//  def projections[A, B, C](f: C ~> A, g: C ~> B) =
//    List(
//      fst ◦ (f &&& g) <-> f,
//      snd ◦ (f &&& g) <-> g
//    )

  // ----------------------
  // Monoidal Category Laws
  // ----------------------

//  @Law
//  def triangleEquation[X, Y] =
//    assocR[X, I, Y] >>> (id[X] ++ snd[I, Y]) <->
//    (fst[I, Y] ++ id[Y])

  @Law
  def pentagonEquation[W: TypeName, X: TypeName, Y: TypeName, Z: TypeName] = ???
    // (assocR[W, X, Y] ++ id[Z]) >>>
    //   assocR[W, (X, Y), Z] >>>
    //   (id[W] ++ assocR[X, Y, Z]) <->
    // (assocR[(W, X), Y, Z] >>> assocR[W, X, (Y, Z)])

  // tensor laws
  //   - on objects:   A ⨂ B = (A, B)
  //   - on morphisms: f ⨂ g = f ++ g
  @Law
  def tensorCompositionLaw[
    A1, A2, B1, B2, C1, C2
  ](
    f1: A1 ~> B1, f2: A2 ~> B2,
    g1: B1 ~> C1, g2: B2 ~> C2,
  ) =
     (g1 ◦ f1) ++ (g2 ◦ f2) <->
     (g1 ++ g2) ◦ (f1 ++ f2)

  @Law
  def tensorIdentityLaw[A: TypeName, B: TypeName] = ???
    // id[A] ++ id[B] <-> id[(A, B)]

  // associator is a natural isomorphism via (a, aInv)
  // ((X, Y), Z) <~> (X, (Y, Z))
  // left and right unitors are natural isomorphisms
  // I ⨂ X <~> X
  // X ⨂ I <~> X

  // -----------------------
  // Symmetric Monoidal Laws
  // -----------------------
  @Law
  def hexagonEquation1[X: TypeName, Y: TypeName, Z: TypeName] = ???
    // (assocL[X, Y, Z] >>> (swap[X, Y] ++ id[Z]) >>>
    //   assocR[Y, X, Z] >>> (id[Y] ++ swap[X, Z]) >>>
    //   assocL[Y, Z, X]) <->
    //   swap[X, (Y, Z)]

  @Law
  def hexagonEquation2[X: TypeName, Y: TypeName, Z: TypeName] = ???
    // (assocR[X, Y, Z] >>> (id[X] ++ swap[Y, Z]) >>>
    //   assocL[X, Z, Y] >>> (swap[X, Z] ++ id[Y]) >>>
    //   assocR[Z, X, Y]) <->
    //   swap[(X, Y), Z]

  @Law
  def symmetry[X: TypeName, Y: TypeName] =
     swap[X, Y] <->
     swapInverse[Y, X]

  // braiding is a natural isomorphism via (b, bInv)
  // (X ⨂ Y) <~> (Y ⨂ X)

}
