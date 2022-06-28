package categories.galois

import annotations1.*
import sets.Orders.Order
import algebra.Homomorphism

case class GaloisConnection[X: Order, Y: Order](
  f: Homomorphism[Order, X, Y],
  g: Homomorphism[Order, Y, X],
) {
  // laws:
  @Law
  def leftAdjoint(x: X, y: Y) =
    if f(x) <= y then x <= g(y) else false

  @Law
  def rightAdjoint(x: X, y: Y) =
    if x <= g(y) then f(x) <= y else false
}

//   X       g      Y
//  ..................
//  g(y) ◀────────  y                           
//    ▲             ▲  
//    │             │  <=
//    │             │                          
//   x   ────────▶ f(x) 
//           f

// f ⊣ g
extension [X: Order, Y: Order] (f: Homomorphism[Order, X, Y]) def ⊣ (g: Homomorphism[Order, Y, X]) = 
  GaloisConnection(f, g)

