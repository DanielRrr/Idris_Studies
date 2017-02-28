module Demo

AdderTypeInt : (numargs : Nat) -> Type
AdderTypeInt Z = Int
AdderTypeInt (S k) = Int -> AdderTypeInt k

adder : (numargs : Nat) -> (acc : Int) -> AdderTypeInt numargs
adder Z t = t
adder (S k) acc = \i => adder k (acc + i)

AdderTypeGeneral : (numargs : Nat) -> (t : Type) -> Type
AdderTypeGeneral Z t = t
AdderTypeGeneral (S k) t = t -> AdderTypeGeneral k t

adderGeneral : Num t => (numargs : Nat) -> (acc : t) -> AdderTypeGeneral numargs t
adderGeneral Z acc = acc
adderGeneral (S k) acc = \i => adderGeneral k (acc+i)
