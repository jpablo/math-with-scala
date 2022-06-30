package applications.processes

import categories.simple.monoidalCategories.props.PortGraph.{singleBox, identity}
import categories.simple.monoidalCategories.props.*
import scala.annotation.alpha
import zio.json.JsonDecoder
import sttp.client3.BodySerializer
import sttp.model.Uri
import isEqual.*


// inline given [A]: TypeName[A] = ${tpeNmeMacro[A]}
// inline given [A]: TypeName[A] = TypeName[A](Single("a"))
inline def tpeName[A](using ev: TypeName[A]): PortLabel = ev.value
// inline def tpeName[A]: PortLabel = ${tpeNmeMacro[A]}

// inline def tpeName2[A]: String = ${tpeNmeMacro2[A]}
// val s = tpeName2[Int]
// val s2 = tpeName2[(Int, String)]
val t = summon[TypeName[(Int, Int)]]

type Graph[A, B] = PortGraph[String]

given PortGraphProcessDSL: ProcessDSL[Graph] with
  def httpCall[A: BodySerializer, B: JsonDecoder](name: String, uri: Uri): Graph[A, B] = ???
//    singleBox[String, String](name, List(tpeName[A]), List(tpeName[B]))


given PortGraphProcessOps: ProcessDSLOps[[A, B] =>> PortGraph[String]] with

  def id[A: TypeName]: Graph[A, A] =
    PortGraph.identity[String]("id", List(tpeName[A]))

  // @alpha("andThen")
  extension [A, B, C] 
    (f: Graph[A, B]) def >>> (g: Graph[B, C]): Graph[A, C] =
      f andThen g

  def fst[A: TypeName, B: TypeName]: Graph[(A, B), A] =
    singleBox[String]("fst", List(tpeName[A], tpeName[B]), List(tpeName[A]))

  def snd[A: TypeName, B: TypeName]: Graph[(A, B), B] =
    singleBox[String]("snd", List(tpeName[A], tpeName[B]), List(tpeName[B]))

  // @alpha("mergeInput")
  extension [A: TypeName, B: TypeName, C: TypeName] (f: A ~> B) 
    def &&& (g: A ~> C): A ~> (B, C) =
      val left = 
        tpeName[A] match {
          case s: Single => List(s)
          case Multiple(lst) => lst
        }
      singleBox[String]("&&&", left, List(tpeName[B], tpeName[C]))
  
  def discard[A: TypeName]: A ~> I =
    singleBox[String]("!", List(tpeName[A]), List.empty)

  def assocR[X: TypeName, Y: TypeName, Z: TypeName]: ((X, Y), Z) ~> (X, (Y, Z)) =
    singleBox[String]("assocR",
      List(Multiple(List(tpeName[X], tpeName[Y])), tpeName[Z]),
      List(tpeName[X], Multiple(List(tpeName[Y], tpeName[Z]))))

  // this is `a >>> swap`
  def assocL[X: TypeName, Y: TypeName, Z: TypeName] : ((X, Y), Z) <~ (X, (Y, Z)) =
    singleBox[String]("assocL",
      List(tpeName[X], Multiple(List(tpeName[Y], tpeName[Z]))),
      List(Multiple(List(tpeName[X], tpeName[Y])), tpeName[Z]))

  def injR[X]: (I, X) <~ X = ???
  def injL[X]: (X, I) <~ X = ???

  // @alpha("combine")
  extension [A1, A2, B1, B2] (f: A1 ~> B1) def ++ (g: A2 ~> B2): (A1, A2) ~> (B1, B2) =
    f ++ g

  def swap[X: TypeName, Y: TypeName]: (X, Y) ~> (Y, X) =
    singleBox[String]("swap", 
      List(tpeName[X], tpeName[Y]), 
      List(tpeName[Y], tpeName[X]))

  def swapInverse[X, Y]: (Y, X) ~> (X, Y) = ???

object ExampleGraph extends App {
  trait A; trait B
  trait C; trait D; trait E; trait F; trait G; trait H; trait J; trait K;

  def process[~>[_, _]](
    p1 : A ~> C,
    p2 : (A, C) ~> D,
    p3 : D ~> (F, G),
    p4 : F ~> H,
    p5 : A ~> J,
    p6 : J ~> K,
    t  : (H, K) ~> B,
  )(using DSL: ProcessDSLOps[~>]) = {
    import DSL.*

    val initial: A ~> ((A, A), A) =
      ∆[A] >>> (id[A] ++ ∆[A]) >>> assocL

    val top: (A, A) ~> H =
      (id[A] ++ p1) >>> p2 >>> p3 >>> fst >>> p4

    val bottom: A ~> K =
      p5 >>> p6

    initial >>> (top ++ bottom) >>> t
  }
  val p1: Graph[A, C]      = singleBox("p1", PortLabel("A"), PortLabel("C"))
  val p2: Graph[(A, C), D] = singleBox("p2", PortLabel("A", "C"), PortLabel("D"))
  val p3: Graph[D, (F, G)] = singleBox("p3", PortLabel("D"), PortLabel("F", "G"))
  val p4: Graph[F, H]      = singleBox("p4", PortLabel("F"), PortLabel("H"))
  val p5: Graph[A, J]      = singleBox("p5", PortLabel("A"), PortLabel("J"))
  val p6: Graph[J, K]      = singleBox("p6", PortLabel("J"), PortLabel("K"))
  val t : Graph[(H,K), B]  = singleBox("t", PortLabel("H", "K"), PortLabel("B"))

  val g = process(p1,p2,p3,p4,p5,p6,t)
}

object Exercise7 {
  trait A
  trait B
  trait C

  def process[~>[_, _]](using DSL: ProcessDSLOps[~>]) = {
    import DSL.*
    type *[A, B] = (A, B)

    type L = (A * B) * C
    type R =  A * (B * C)

    // ---------------------------------------
    val a: L ~> A = fst[A * B, C] >>> fst[A, B]
    val b: L ~> B = fst[A * B, C] >>> snd[A, B]
    val c: L ~> C = snd[A * B, C]

    val from: L ~> R = a &&& (b &&& c)
    // ---------------------------------------
    val x: R ~> A = fst[A, B * C]
    val y: R ~> B = snd[A, B * C] >>> fst[B, C]
    val z: R ~> C = snd[A, B * C] >>> snd[B, C]
    
    val to: R ~> L = (x &&& y) &&& z

    // -----------------------------
    // goal:
      (from >>> to) <-> id[(A * B) * C]
      // -----------------------------

    val from2: L ~> R = 
      ((fst[A * B, C] >>> fst[A, B]) &&& ((fst[A * B, C] >>> snd[A, B]) &&& snd[A * B, C]))
    
    val to2: R ~> L = 
      ((fst[A, B * C] &&& (snd[A, B * C] >>> fst[B, C])) &&& (snd[A, B * C] >>> snd[B, C]))

    
    from >>> to
    (b &&& c)
    a
  }

  val g = process
}


object triangle_equations {
  trait X
  trait Y

  def process[~>[_, _]](using DSL: ProcessDSLOps[~>]) = {
    import DSL.*
    type *[A, B] = (A, B)

    def l[A: TypeName] = snd[I, A]
    def r[A: TypeName] = fst[A, I]
    def a[A: TypeName, B: TypeName, C: TypeName] = assocR[A, B, C]

    (a[X, I, Y] >>> (id[X] ++ l[Y])) <-> r[X] ++ id[Y]
  }

  val g = process
}

object pentagon_equations {
  trait X
  trait Y
  trait Z
  trait W

  def process[~>[_, _]](using DSL: ProcessDSLOps[~>]) = {
    import DSL.*
    type *[A, B] = (A, B)

    def l[A: TypeName] = snd[I, A]
    def r[A: TypeName] = fst[A, I]
    def a[A: TypeName, B: TypeName, C: TypeName] = assocR[A, B, C]

    (a[W, X, Y] ++ id[Z]) >>> a[W, X * Y, Z] >>> (id[W] ++ a[X, Y, Z]) <-> (a[W * X, Y, Z] >>> a[W, X, Y * Z])    
    // ((a[W, X, Y] ++ id[Z]) >>> a[W, X * Y, Z]) >>> (id[W] ++ a[X, Y, Z])
    // (a[W * X, Y, Z] >>> a[W, X, Y * Z])
  }

  val g = process
}

