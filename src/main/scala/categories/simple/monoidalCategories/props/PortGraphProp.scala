package categories.simple.monoidalCategories.props

import java.util.UUID

import applications.processes.{PortLabel, Single, Multiple}
import categories.simple.CategoryS
import categories.simple.monoidalCategories.props.PortGraph.singleBox

import scala.collection.immutable.SeqMap
import com.devskiller.friendly_id.FriendlyId

type BoxId = String

def mkId = FriendlyId.createFriendlyId

def withId[B](b: B): Box[B] = (b, mkId)

type PortGraphCat[B] =
  [X, Y] =>> PortGraph[B]

type Box[B] = (B, BoxId)
// This would be called Connection[B], since the port is attached to the box
type Port[B] = (Box[B], Int)
type BoxWithPorts[B] = SeqMap[Box[B], (List[PortLabel], List[PortLabel])]
type ->[A, B] = (A, B)

case class PortGraph[B](
  boxes: BoxWithPorts[B],
  // connections:
  incoming   : List[Port[B]],
  inner      : List[Port[B] -> Port[B]],
  outgoing   : List[Port[B]],
) {
  infix def andThen(other: PortGraph[B]) = {
    val (boxes, inner, incoming, outgoing) = PortGraph.combineEdges(this, other)
    PortGraph(
      boxes      = boxes,
      incoming   = incoming,
      inner      = inner,
      outgoing   = outgoing,
    )
  }
  def ++(other: PortGraph[B]) =
    PortGraph(
      boxes      = boxes    ++ other.boxes,
      incoming   = incoming ++ other.incoming,
      inner      = inner    ++ other.inner,
      outgoing   = outgoing ++ other.outgoing,
    )
}

object PortGraph {
  enum Direction derives CanEqual:
    case Out
    case In

  def attachedToIdentity[B](p: Port[B]) = p._1._1.toString == "id"

  // flatten the first level of inlined 2-tuples. i.e. (A, (B, (C, D))) => (A, B, (C, D))
  def expandTuple2[B](connections: List[Port[B]], boxes0: BoxWithPorts[B], dir: Direction): (List[Port[B]], BoxWithPorts[B]) = {

    val z0 = (
      List.empty[Port[B]],
      Map.empty[Box[B], Int].withDefaultValue(0),
      boxes0
    )

    def go(box: Box[B], pos0: Int, label: PortLabel): List[Port[B] -> PortLabel] =
      label match {
        case _: Single => List((box, pos0) -> label)
        case Multiple(labels) =>
          val z0 = (pos0, List.empty[Port[B] -> PortLabel])
          val (_, r) =
            labels.foldLeft(z0) { case ((pos, acc), label) =>
              val acc1 = go(box, pos, label)
              (pos + acc1.length, acc1 ++ acc)
            }
          r
      }

    val (updatedConnections, _, updatedBoxes) =
      connections.foldLeft(z0) { case ((connectionsAcc: List[Port[B]], insertedLabelCounts, boxes), connection @ (box, pos)) =>
        val (left, right) = boxes(box)
        val labels = if (dir == Direction.In) left else right
        // the number extra ports inserted so far
        val delta = insertedLabelCounts(box)
        val startPos = pos + delta

        val label = labels(startPos)

        // go(b, 1, "E") -> List((b, 1) -> "E", ...)
        val expanded: List[Port[B] -> PortLabel] = go(box, startPos, label)

        assert(expanded.nonEmpty)

        val newDelta = delta + expanded.size - 1
        val (newConnections, expandedLabels) = expanded.unzip
        // now we have to splice the expand labels into the existing labels
        val (before, current :: after) = labels.splitAt(startPos) : @unchecked
        val newLabels = before ++ expandedLabels.reverse ++ after
        val newLabelsBoth = (if (dir == Direction.In) (newLabels, right) else (left, newLabels))

        (
          newConnections ++ connectionsAcc,
          insertedLabelCounts + (box -> newDelta),
          boxes + (box -> newLabelsBoth)
        )
      }

    (updatedConnections.reverse, updatedBoxes)
  }

  def combineEdges[B](g1: PortGraph[B], g2: PortGraph[B]) = {

    // sanity check: graphs are compatible
    val left0  = g1.outgoing.map { case (b, i) => g1.boxes(b)._2(i) }
    val (g1Outgoing, g1Boxes) = expandTuple2(g1.outgoing, g1.boxes, Direction.Out)
    val left  = g1Outgoing.map { case (b, i) => g1Boxes(b)._2(i) }


    val right0 = g2.incoming.map { case (b, i) => g2.boxes(b)._1(i) }
    val (g2Incoming, g2Boxes) = expandTuple2(g2.incoming, g2.boxes, Direction.In)
    val right = g2Incoming.map { case (b, i) => g2Boxes(b)._1(i) }

    assert(left == right, s"$left != $right")


    // -------------
    // initial data:
    // -------------
    // - inner connections
    val initialConnections = g1.inner ++ g2.inner
    // - boxes in inner connections
    val initialBoxesLeft: BoxWithPorts[B] =
      SeqMap.from {
        g1.inner.flatMap { case ((p1, _), (p2, _)) =>
          Seq(p1 -> g1Boxes(p1), p2 -> g1Boxes(p2))
        }
      }
    val initialBoxesRight: BoxWithPorts[B] =
      SeqMap.from {
        g2.inner.flatMap { case ((p1, _), (p2, _)) =>
          Seq(p1 -> g2Boxes(p1), p2 -> g2Boxes(p2))
        }
      }
    val initialBoxes: BoxWithPorts[B] = initialBoxesLeft ++ initialBoxesRight

    // - ports attached to inner boxes
    val initialIncoming = g1.incoming.filter(p => initialBoxes.contains(p._1))
    val initialOutgoing = g2.outgoing.filter(p => initialBoxes.contains(p._1))

    val initial =
      ( initialBoxes,
        initialConnections,
        initialIncoming.reverse,
        initialOutgoing.reverse
        )

    val r =
      g1Outgoing.indices.foldLeft(initial) { case ((boxes, inner, incoming, outgoing), i) =>
        val left:  Port[B] = g1Outgoing(i)
        val right: Port[B] = g2Incoming(i)
        val rightBox = right._1
        val leftBox = left._1
        val rightBoxWithPorts = rightBox -> g2Boxes(rightBox)
        val leftBoxWithPorts = leftBox -> g1Boxes(leftBox)
        val outPorts = g2.outgoing.filter(port => port._1 == rightBox)
        val inPorts = g1.incoming.filter(port => port._1 == leftBox)

        if (attachedToIdentity(left)) {
          // |-1-| ++ |-g-|
          (
            boxes + rightBoxWithPorts,
            inner,
            right :: incoming,
            outPorts.reverse ++ outgoing )

        } else if (attachedToIdentity(right)) {
          // |-f-| ++ |-1-|
          (
            boxes + leftBoxWithPorts,
            inner,
            inPorts.reverse ++ incoming,
            left :: outgoing )

        } else // |-f-| ++ |-g-|
          ( boxes + rightBoxWithPorts + leftBoxWithPorts,
            left -> right :: inner,
            inPorts.reverse ++ incoming,
            outPorts.reverse ++ outgoing )
      }
    (r._1, r._2.reverse.distinct, r._3.reverse.distinct, r._4.reverse.distinct)
  }

  def emptyGraph[B] =
    PortGraph[B](SeqMap.empty, List.empty, List.empty, List.empty)

  // A single box with wires
  def singleBox[B](b: B, in: List[PortLabel], out: List[PortLabel]): PortGraph[B] = {
    val box = withId(b)
    PortGraph(
      SeqMap(box -> (in, out)),
      incoming = in.indices.map(box -> _).toList,
      inner = List.empty,
      outgoing = out.indices.map(box -> _).toList,
    )
  }

  // identity Port Graph
  def identity[B](box: B, ps: List[PortLabel]): PortGraph[B] =
    ps.foldLeft(emptyGraph) { (g, p) =>
      // copy the box label for now, but there should be a function
      // P => B  (i.e. id[P]: P ~> P)
      g ++ singleBox[B](box, List(p), List(p))
    }
}

object Examples extends App {
  import PortGraph.*

  val a = withId("a")
  val b = withId("b")
  val c = withId("c")

  val graph1 =
    PortGraph(
      boxes = SeqMap(
        a -> (PortLabel("A"),         PortLabel("B","C","D")),
        b -> (PortLabel("D","C","G"), PortLabel("Y","E","F")),
        c -> (PortLabel("B","Y"),     PortLabel("Z")),
      ),
      incoming = List((a, 0), (b, 2)),
      inner = List(
        (a, 0) -> (c, 0),
        (a, 1) -> (b, 1),
        (a, 2) -> (b, 0),
        (b, 0) -> (c, 1),
      ),
      outgoing = List((c, 0), (b, 1), (b, 2))
    )
  val graph2 = singleBox("x", PortLabel("Z", "E", "F"), PortLabel("W"))
  val combined = graph1 andThen graph2

  val idRight = identity[String]("id", PortLabel("W"))
  val idLeft = identity[String]("id", PortLabel("Z", "E", "F"))

  // sanity check
//  val combinedRight = graph2 andThen idRight
//  val combinedLeft = idLeft andThen graph2
//  assert(combinedRight == graph2)
//  assert(combinedLeft == graph2)
}

object Presentation {
  import PortGraph.*
  val process = singleBox("p", PortLabel("A"), PortLabel("B", "C"))
  def sort     = singleBox("sort", PortLabel("List[A]"), PortLabel("List[A]"))
  val cooking  = singleBox("cooking", PortLabel("eggs", "butter", "cheese"), PortLabel("omelette"))
  val equation = singleBox("x² + x²", PortLabel("ℝ", "ℝ"), PortLabel("ℝ"))
  val usbCord  = singleBox("usb cord", PortLabel("Microphone"), PortLabel("Computer"))
  val matrix  = singleBox("         ", PortLabel("3"), PortLabel("2"))

  // parallel composition
  val par1 = singleBox("x", PortLabel("A", "B"), PortLabel("C"))
  val par2 = singleBox("y", PortLabel("D"), PortLabel("E"))
  val parallel = par1 ++ par2

  // sequential composition
  val seq0 =
    singleBox("h1", PortLabel("A"), PortLabel("B")) ++ singleBox("h2", PortLabel("C"), PortLabel("D"))

  val f = withId("f")
  val g = withId("g")
  val seq1 =
    PortGraph[String](
      boxes = SeqMap(
        f -> (PortLabel("D"), PortLabel("A")),
        g -> (PortLabel("B", "A"), PortLabel("E", "F")),
      ),
      incoming = List((g, 0), (f, 0)),
      inner    = List((f, 0) -> (g, 1)),
      outgoing = List((g, 0), (g, 1))
    )

  val sequential = seq0 andThen seq1
  // diagram equations

  val diagramEq2 =
    singleBox("f", PortLabel("A"), PortLabel("B")) ++
    singleBox("g", PortLabel("C"), PortLabel("D"))

  // process equations
  val sort2 = sort andThen sort
  val zeroAdd = singleBox("+", PortLabel("0", "m"), PortLabel(""))
  val identityM = identity("id", PortLabel("m"))
}
