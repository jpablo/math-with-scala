package applications.processes

import scala.quoted.*

case class TypeName[A](value: PortLabel)

object TypeName {

  inline given emptyTypeName: TypeName[EmptyTuple] = TypeName(Single("I"))

  inline given [A] => TypeName[A] = ${ tpeNmeMacro[A] }

  // inline given [A, B](using na: TypeName[A], nb: TypeName[B]): TypeName[(A, B)] =
  // TypeName(Multiple(List(na.value, nb.value)))
}

object PortLabel {
  def apply(lst: String*): List[PortLabel] =
    lst.toList.map(Single.apply)

  // A bare string can be used wherever an `into[List[PortLabel]]` is expected (SIP-71)
  given Conversion[String, List[PortLabel]] = s => List(Single(s))
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

given ToExpr[PortLabel]:
  def apply(x: PortLabel)(using Quotes) =
    x match {
      case Single(value) => '{ Single(${ Expr(value) }) }
      case Multiple(value) => '{ Multiple(${ Expr(value) }) }
    }

/**
 * Creates a short representation of the type A
 */
def tpeNmeMacro[A: Type](using q: Quotes): Expr[TypeName[A]] = {
  import quotes.reflect.*

  // The literal string types in a named tuple's names tuple, e.g. ("price", "qty")
  def fieldNames(x: TypeRepr): List[String] =
    x.dealias match {
      case ConstantType(StringConstant(s)) => List(s)
      case AppliedType(tpt, args) if defn.isTupleClass(tpt.typeSymbol) =>
        args.flatMap(fieldNames)
      case AppliedType(tpt, List(head, tail)) if tpt.typeSymbol.name == "*:" =>
        fieldNames(head) ++ fieldNames(tail)
      case _ => Nil
    }

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
      // Named tuples (SIP-58): ports are labeled by field name rather than type name
      case AppliedType(tpt, List(names, _)) if tpt.typeSymbol.name == "NamedTuple" =>
        fieldNames(names).map(Single.apply) match {
          case single :: Nil => single
          case labels => Multiple(labels)
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
  '{ TypeName[A](${ Expr(toTypeName(TypeRepr.of[A])) }) }
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
