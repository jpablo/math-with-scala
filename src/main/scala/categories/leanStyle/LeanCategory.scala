package categories.leanStyle
import annotations1.*
import isEqual.{IsEq, <->}

// class has_hom (obj : Type u) : Type (max u (v+1)) :=
// (hom : obj → obj → Type v)

trait Quiver[V]:
  type Hom[_ <: V, _ <: V]
  type ~> [A <: V, B <: V] = Hom[A, B]

trait HasHom[Obj]:
  type Hom[A <: Obj, B <: Obj]
  type ~> [A <: Obj, B <: Obj] = Hom[A, B]

trait CategoryStruct[Obj] extends Quiver[Obj]:

  // (id : Π X : obj, hom X X)
  def id[X <: Obj]: X ~> X
  type Id[X <: Obj] <: X ~> X

  // (comp : Π {X Y Z : obj}, (X ⟶ Y) → (Y ⟶ Z) → (X ⟶ Z))
  extension [X <: Obj, Y <: Obj, Z <: Obj]
    (f: X ~> Y) def >>> (g: Y ~> Z): X ~> Z

//  type AndThen[X <: Obj, Y <: Obj, Z <: Obj, F[]]

end CategoryStruct

trait Category[Obj] extends CategoryStruct[Obj]:
  // (id_comp' : ∀ {X Y : obj} (f : hom X Y), 𝟙 X ≫ f = f . obviously)
//  def id_comp [X <: Obj, Y <: Obj, F[_ <: Obj, _ <: Obj]]: id[X] >>> F = F

  @Law
  def associativity[A <: Obj, B <: Obj, C <: Obj, D <: Obj](
    f: A ~> B,
    g: B ~> C,
    h: C ~> D
  ) =
    ((f >>> g) >>> h) <-> (f >>> (g >>> h))

end Category

// -----------
// Cat
// -----------

type Cat = Bundled[Category]

def str(c: Cat): Category[c.A] = c.str
