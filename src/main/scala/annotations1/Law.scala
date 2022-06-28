package annotations1

case class Law(description: String = "") extends scala.annotation.StaticAnnotation

case class Proof(description: String = "") extends scala.annotation.StaticAnnotation
