package render.graphivz

import java.io.File
import java.util.UUID

import categories.simple.monoidalCategories.props.{PortGraph, BoxId}
import guru.nidi.graphviz.attribute.{Attributes, Color, Label, Rank, Shape, Style, Size}
import guru.nidi.graphviz.attribute
import guru.nidi.graphviz.attribute.Rank.RankType
import guru.nidi.graphviz.model.Factory.{graph, node, to}
import guru.nidi.graphviz.model.{Compass, Graph, Node}
import guru.nidi.graphviz.model.Factory.*
import guru.nidi.graphviz.attribute.Font
import guru.nidi.graphviz.attribute.Rank
import guru.nidi.graphviz.attribute.Style
import guru.nidi.graphviz.engine.Format
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.attribute.Records
import guru.nidi.graphviz.attribute.Records.*
import scala.collection.immutable.SeqMap

object RenderGraphvizHtml {


  def render[B, P](pg: PortGraph[B]): Graph = {
    println(pg.boxes)
    println(pg.incoming)
    println(pg.inner)
    println(pg.outgoing)

    val nodes: SeqMap[(B, BoxId), Node] =
      pg.boxes.transform { case ((v, id), (inPorts, outPorts)) =>
        val id = UUID.randomUUID.toString
        // hack
        if (v.toString == "id")
          println(v)
          node(id).`with`(Shape.POINT)
        else
          toNode(
            box(v.toString, inPorts.map(_.toString), outPorts.map(_.toString), showPorts = false).toString,
            id
          )
      }

    val innerLinks: List[Node] =
      pg.inner.map { case t @ ((b1, p1), (b2, p2)) =>
        val label1 = pg.boxes(b1)._2(p1).toString
        val label2 = pg.boxes(b2)._1(p2).toString
        val label = if (label1 == label2) label1 else label1 + ":" + label2
        linkInner(
          nodes(b1), p1.toString,
          nodes(b2), p2.toString,
         label
        )
      }

    val (incoming, incomingLinks) =
      pg.incoming.zipWithIndex.map { case ((b, p), i) =>
        val n = node("i" + i).`with`(Shape.POINT, Color.WHITE.fill())
        val label = pg.boxes(b)._1(p)
        (n, linkIncoming(n, nodes(b), p.toString, label.toString))
      }.unzip

    val (outgoing, outgoingLinks) =
      pg.outgoing.zipWithIndex.map { case ((b, p), i) =>
        val n = node("o" + i).`with`(Shape.POINT, Color.WHITE.fill())
        val label = pg.boxes(b)._2(p)
        (n, linkOutgoing(nodes(b), p.toString, n, label.toString))
      }.unzip

    val incomingSortLinks =
      if (incoming.length > 1) List(incoming.reduceRight((n1, n2) => n1 link to(n2).`with`(Style.INVIS)))
      else incoming

    val outgoingSortLinks =
      if (outgoing.length > 1) List(outgoing.reduceRight((n1, n2) => n1 link to(n2).`with`(Style.INVIS)))
      else outgoing


    graph("render").`with`(
      graph("incoming")
        .graphAttr().`with`(Rank.inSubgraph(RankType.SAME))
        .`with`(incomingSortLinks:_*)
      ,
      graph("outgoing")
        .graphAttr()
        .`with`(Rank.inSubgraph(RankType.SAME))
        .`with`(outgoingSortLinks:_*)
    )
    .`with`(incomingLinks:_*)
    .`with`(innerLinks:_*)
    .`with`(outgoingLinks:_*)
  }

  type PortId = String

  def box(boxLabel: String, inPorts: List[PortId], outPorts: List[PortId], showPorts: Boolean = false) = {
    import scalatags.Text.all.*
    import scala.language.implicitConversions

    // Graphviz specific attributes
    val border  = attr("BORDER")
    val cBorder  = attr("CELLBORDER")
    val cPadding = attr("CELLPADDING")
    val cSpacing = attr("CELLSPACING")
    val port     = attr("PORT")
    val bgColor     = attr("BGCOLOR")

    def portsTable(prefix: String, ports: Range) =
      table(border := 0, cPadding := 2, cSpacing := 1,
        if ports.nonEmpty
        then ports.map(id => tr(td(port := (prefix + id), if (showPorts) id else "")))
        else tr(td(""))
      )

    table(border := 0, cBorder := 0, cPadding := 0, cSpacing := 0, //style:="ROUNDED", bgColor := "LIGHTBLUE",
      tr(
        td(portsTable("i", inPorts.indices)),
        td(
          table(border := 0, cBorder := 0, cPadding := 1,
            tr(td("")),
            tr(td(boxLabel)),
            tr(td(""))
          )
        ),
        td(portsTable("o", outPorts.indices))
      )
    )
  }
  // guru.nidi.graphviz.attribute.Size
  def toNode(label: String, id: String) =
    node(id).`with`(
      Label.html(label),
      Shape.PLAIN,
      Style.FILLED,
////      Style.ROUNDED,
      Color.LIGHTBLUE
    )

  def linkInner(n0: Node, p0: String, n1: Node, p1: String, label: String = "") =
    n0 link between(port("o" + p0, Compass.EAST), n1.port("i" + p1, Compass.WEST))
      .`with`(attribute.Arrow.NONE, attribute.Label.of(label))

  def formatType(s: String) = {
    val tupleRegex = """.*Tuple2\[([^,]*),([^,]*)\]""".r
  }

  def linkIncoming(n0: Node, n1: Node, p1: String, label: String = "") =
    n0 link to(n1.port("i" + p1, Compass.WEST))
      .`with`(attribute.Arrow.NONE, attribute.Label.of(label))

  def linkOutgoing(n0: Node, p0: String, n1: Node, label: String = "") =
    n0 link between(port("o" + p0, Compass.EAST), n1)
      .`with`(attribute.Arrow.NONE, attribute.Label.of(label))

  def linkIdentity(n0: Node, n1: Node, label: String = "") =
    n0 link to(n1)
      .`with`(attribute.Arrow.NONE, attribute.Label.of(label))

}
