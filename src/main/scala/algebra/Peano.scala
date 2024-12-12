package algebra

trait Peano[N]:
  val zero: N
  def s(n: N): N

  def `s is injective`(n: N, m: N) =
    (if n == m then s(n) == s(m) else true) &&
      (if s(n) == s(m) then n == m else true)

  def `s is not a successor`(n: N) =
    s(n) != zero

object PeanoInstances:
  import java.math.BigInteger

  case class Nats(value: BigInteger)
//    assert(value.compareTo(BigInteger.ZERO) >= 0)

  given Peano[Nats]:
    val zero = Nats(BigInteger.ZERO)
    def s(n: Nats) = Nats(n.value.add(BigInteger.ONE))
