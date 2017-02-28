module Demo

import Data.Vect

three : Vect 3 (Double, Double)
three = [(0.0, 0.0), (1.1, 1.1), (2.2, 2.2)]

Position : Type
Position = (Double, Double)

three' : Vect 3 Position
three' = [(0.0, 0.0), (1.1, 1.1), (2.2, 2.2)]

Polygon : Nat -> Type
Polygon n = Vect n Position
