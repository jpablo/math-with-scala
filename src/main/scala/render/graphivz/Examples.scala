package render.graphivz

import guru.nidi.graphviz.engine.Format
import guru.nidi.graphviz.engine.Graphviz
import guru.nidi.graphviz.model.{Compass, Graph, Node}
import guru.nidi.graphviz.model.Factory.{graph, node, to}
import guru.nidi.graphviz.attribute.{Attributes, Color, Label, Rank, Shape, Style}
import java.io.File

object Examples extends App:

  // --------------------------
  val g: Graph =
    graph("outer")
      .directed
      .graphAttr().`with`(Rank.dir(Rank.RankDir.LEFT_TO_RIGHT))
//      .graphAttr().`with`("nodesep", "0.05")
//      .graphAttr().`with`(Attributes.attr("ordering", "out"))


  val diagrams =
    List(
//      "idLeft"                -> categories.simple.monoidalCategories.props.Examples.idLeft,
//       "idRight"               -> categories.simple.monoidalCategories.props.Examples.idRight,
      // "graph1"                -> categories.simple.monoidalCategories.props.Examples.graph1,
      // "graph2"                -> categories.simple.monoidalCategories.props.Examples.graph2,
      // "combined"              -> categories.simple.monoidalCategories.props.Examples.combined,
//       "process"               -> categories.simple.monoidalCategories.props.Presentation.process,
      // //      "combinedRight" -> categories.simple.monoidalCategories.props.Examples.combinedRight,
      // //      "combinedLeft"  -> categories.simple.monoidalCategories.props.Examples.combinedLeft,
      // "sort"                  -> categories.simple.monoidalCategories.props.Presentation.sort,
      // "cooking"               -> categories.simple.monoidalCategories.props.Presentation.cooking,
      // "equation"              -> categories.simple.monoidalCategories.props.Presentation.equation,
      // "usb-cord"              -> categories.simple.monoidalCategories.props.Presentation.usbCord,
      // "matrix"                -> categories.simple.monoidalCategories.props.Presentation.matrix,
      // "parallel1"             -> categories.simple.monoidalCategories.props.Presentation.par1,
      // "parallel2"             -> categories.simple.monoidalCategories.props.Presentation.par2,
      // "parallel"              -> categories.simple.monoidalCategories.props.Presentation.parallel,
      // "seq0"                  -> categories.simple.monoidalCategories.props.Presentation.seq0,
      // "seq1"                  -> categories.simple.monoidalCategories.props.Presentation.seq1,
      // "sequential"            -> categories.simple.monoidalCategories.props.Presentation.sequential,
      // "sort2"                 -> categories.simple.monoidalCategories.props.Presentation.sort2,
      // "diagramEq2"            -> categories.simple.monoidalCategories.props.Presentation.diagramEq2,
      // "zeroAdd"               -> categories.simple.monoidalCategories.props.Presentation.zeroAdd,
      // "identityM"             -> categories.simple.monoidalCategories.props.Presentation.identityM,
//       "example"               -> applications.processes.ExampleGraph.g,
      // "exercise7"             -> applications.processes.Exercise7.g,
       "triangleEquations1"    -> applications.processes.triangle_equations.g.lhs,
      // "triangleEquations2"    -> applications.processes.triangle_equations.g.rhs,
      // "pentagonEquations0"    -> applications.processes.pentagon_equations.g,
//       "pentagonEquations1"    -> applications.processes.pentagon_equations.g.lhs,
      // "pentagonEquations2"    -> applications.processes.pentagon_equations.g.rhs,
    )

  diagrams.foreach { case (name, diagram) =>
    println(name)
//    println(diagram)
    val viz = Graphviz.fromGraph(
      g.`with`(RenderGraphvizHtml.render(diagram))
    ).width(1024)

    println(s"----- $name -----")
    println(viz.render(Format.DOT).toString)

//    viz.render(Format.PNG).toFile(new File(s"example/$name.png"))
//    viz.render(Format.SVG).toFile(new File(s"example/$name.svg"))
  }

