package categories.simple

import isEqual.*
import categories.simple.{*, given}
import annotations1.*


// object Equalizer

// ---------
// equalizer
// ---------
trait Equalizer
  [X, Y, ~>[_, _]: Category]
  (f: X ~> Y, g: X ~> Y):
  
    case class Equalized[W](value: W ~> X) { def law = f ◦ value <-> g ◦ value }
  
    type E
    val m: Equalized[E]
  
    // Universal property
    def unique[Z](h: Equalized[Z]): Z ~> E
  
    def universalProp[Z](h: Equalized[Z]) =
      m.value ◦ unique(h) <-> h.value
      
end Equalizer
