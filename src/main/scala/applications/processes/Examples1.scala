package applications.processes

import categories.simple.monoidalCategories.{CartesianMonoidalCategory, MonoidalCategory}
import categories.simple.Category
import zio.RIO

object Examples1:
  trait A; trait B
  trait X1;trait X2;trait X3;trait X4;trait X5;trait X6;trait X7;trait X8;
  
  def process1[~>[_, _]](
    C: Category[~>]
  ) = {
    import C.*
    id[X1]
  }

  def process2[~>[_, _]](
    CMC: CartesianMonoidalCategory[~>, Tuple2],
    p1 : A ~> X1,
    p2 : (A, X1) ~> X2,
    p3 : X2 ~> (X4, X5),
    p4 : X4 ~> X6,
    p5 : A ~> X7,
    p6 : X7 ~> X8,
    t  : (X6, X8) ~> B,
  ): A ~> B = {
    import CMC.*
    
    val initial = Δ >>> (id ** Δ) >>> aI
      : A ~> ((A, A), A)
    val top = (id ** p1) >>> p2  >>> p3 >>> fst >>> p4
      : (A, A) ~> X6
    val bottom = p5 >>> p6
      : A ~> X8
    initial >>> (top ** bottom) >>> t
      : A ~> B
  }
