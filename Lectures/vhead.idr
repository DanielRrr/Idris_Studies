module Demo

import Data.Vect

vhead : Vect (1 + n) a -> a
vhead (x :: _) = x

vhead' : (n : Nat) -> (a : Type) -> Vect (1 + n) a -> a
vhead' n a (x :: _) = x
