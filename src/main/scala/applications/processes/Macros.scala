package applications.processes

import scala.quoted.*

case class TypeName[A](value: PortLabel)

object TypeName {

  inline given emptyTypeName: TypeName[EmptyTuple] = TypeName(Single("I"))
  inline given [A]: TypeName[A] = ${tpeNmeMacro[A]}

  // inline given [A, B](using na: TypeName[A], nb: TypeName[B]): TypeName[(A, B)] =
    // TypeName(Multiple(List(na.value, nb.value)))
}

object PortLabel {
  def apply(lst: String*): List[PortLabel] =
    lst.toList.map(Single.apply)
}

sealed trait PortLabel {
  def asList =
    this match {
      case s: Single => List(s)
      case Multiple(lst) => lst
    }


  override def toString: String =
    this match {
      case Single(value) => value
      case Multiple(value) => value.map(_.toString).mkString("(", ", ", ")")
    }
}
case class Single(value: String) extends PortLabel
case class Multiple(value: List[PortLabel]) extends PortLabel

given ToExpr[PortLabel] with {
  def apply(x: PortLabel)(using Quotes) =
    x match {
      case Single(value) => '{ Single(${Expr(value)})  }
      case Multiple(value) => '{ Multiple(${Expr(value)})  }
    }
}

/**
 * Creates a short representation of the type A
 */
def tpeNmeMacro[A: Type](using q: Quotes): Expr[TypeName[A]] = {
  import quotes.reflect.*

  def toTypeName(x: TypeRepr): PortLabel = {
    // println(x.show)
    x match {
      case AppliedType(tpt, args: List[TypeRepr]) if tpt.typeSymbol == defn.TupleClass(2) =>
        Multiple {
          args.map {
            case TypeRef(_, b) => Single(b)
            case x => toTypeName(x)
          }
        }
      case TypeRef(_, b) =>
        // println("--- TypeRef(_, b) ---")
        // println(x)
        Single(b)
      case y =>
        // println("--- ? ---")
        println(x)
        Single("?")
    }
  }
  // Expr(toTypeName(TypeRepr.of[A]))
  '{ TypeName[A](${Expr(toTypeName(TypeRepr.of[A]))})  }
  // '{ TypeName[A](${Expr(toTypeName(t.unseal.tpe))})  }
  // '{ Single("a") }
  // Expr(t.show)
}

def tpeNmeMacro2[A: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  val r = TypeRepr.of[A].dealias.simplified
//  println(s"------- tpeNmeMacro2: ${r.showAnsiColored}--------")
//  println(r.showExtractors)
  println(r.typeSymbol)
  Expr(r.show)
