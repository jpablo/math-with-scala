package applications.processes

import categories.simple.Category
import categories.simple.monoidalCategories.{CartesianMonoidalCategory, MonoidalCategory}
import zio.RIO

object ExamplesProcess:
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
  )(using DSL: ProcessDSLOps[~>]) : A ~> B = {
    import DSL.*

    val initial: A ~> ((A, A), A) =
      ∆[A] >>> (id[A] ++ ∆[A]) >>> assocL

    val top: (A, A) ~> H =
      (id[A] ++ p1) >>> p2 >>> p3 >>> fst >>> p4

    val bottom: A ~> K =
      p5 >>> p6

    initial >>> (top ++ bottom) >>> t
  }
