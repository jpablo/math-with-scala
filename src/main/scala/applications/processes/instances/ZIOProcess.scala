package applications.processes.instances

import scala.annotation.alpha
import applications.processes.{ProcessDSL, ProcessDSLOps, TypeName}
import zio.{RIO, Task, ZIO}
import java.net.URI

import io.circe.Decoder
import sttp.client3.HttpURLConnectionBackend
import sttp.model.Uri

given RIOProcessDSL: ProcessDSL[RIO] with
  val backend = HttpURLConnectionBackend()
  import sttp.client3.*
  import sttp.client3.circe.*
  
  def httpCall[A: BodySerializer, B: Decoder](name: String, uri: Uri): RIO[A, B] =
    RIO.fromFunctionM[A, B] { (a: A) =>
      RIO.fromEither {
        basicRequest
          .post(uri)
          .body(a)
          .response(asJson[B])
          .send(backend)
          .body
          .left.map(new Exception(_))
      }
    }

given RIOProcessOps: ProcessDSLOps[RIO] =  ???
//  def id[A] = RIO.identity[A]
//
//  // @alpha("andThen")
//  extension [A, B, C] (f: RIO[A, B]) def >>> (g: RIO[B, C]) = f andThen g
//
//  def fst[A: TypeName, B: TypeName]: RIO[(A, B), A] = RIO.first
//  def snd[A, B]: RIO[(A, B), B] = RIO.second
//
//  // @alpha("mergeInput")
//  extension [A, B, C] (f: RIO[A, B]) def &&& (g: RIO[A, C]): RIO[A, (B, C)] = f &&& g
//
//  def discard[A]: RIO[A, I] = RIO.succeed(EmptyTuple)
//  def assocR[X, Y, Z]    : ((X, Y), Z) ~> (X, (Y, Z)) = RIO.fromFunction { case ((x, y), z) => (x, (y, z)) }
//  def assocL[X, Y, Z] : ((X, Y), Z) <~ (X, (Y, Z)) = RIO.fromFunction { case (x, (y, z)) => ((x, y), z) }
//  def injR[X]: X ~> (I, X) = RIO.fromFunction { a => (EmptyTuple, a) }
//  def injL[X]: X ~> (X, I) = RIO.fromFunction { a => (a, EmptyTuple) }
//
//  // @alpha("combine")
//  extension [A1, A2, B1, B2] (f: A1 ~> B1) def ++ (g: A2 ~> B2): (A1, A2) ~> (B1, B2) = ???
////    val left  = fst >>> f
////    val right = snd >>> g
////    (left zipWithPar right) ((x, y) => (x, y))
//
//  def swap[X, Y]   : (X, Y) ~> (Y, X) = RIO.swap
//  def swapInverse[X, Y]: (X, Y) <~ (Y, X) = RIO.swap 


