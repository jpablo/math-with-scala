package isEqual

final case class IsEq[A](lhs: A, rhs: A)

extension [A] (lhs: A) def <-> (rhs: A) = IsEq(lhs, rhs)
