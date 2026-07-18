package applications.examples

/**
 * "Better fors" (SIP-62), a standard feature as of Scala 3.9.
 *
 *   1. A `for` may now start with pure aliases, before the first generator.
 *   2. A trailing `yield` of the last bound variable desugars without the
 *      extra `map(identity)` step: `for n <- xs yield n` is just `xs`.
 *
 * Composing Kleisli arrows for the Option monad (see docs/kleisli.md).
 */
object BetterFors:

  val parse: String => Option[Int]    = _.toIntOption
  val recip: Int    => Option[Double] = n => if n == 0 then None else Some(1.0 / n)

  // (1) leading alias: `scale` is defined before any generator
  def pipeline(input: String): Option[Double] =
    for
      scale = 100.0
      n <- parse(input)
      x <- recip(n)
    yield scale * x

  // (2) desugars to parse(s) directly — no trailing map(identity)
  def parsed(s: String): Option[Int] =
    for n <- parse(s) yield n
