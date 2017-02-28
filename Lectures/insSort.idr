module Demo

import Data.Vect

insert : (x : Nat) -> (xsSorted : Vect len Nat) -> Vect (S len) Nat
insert x [] = [x]
insert x ys@(y :: xs) = case x < y of
                          False => y :: (insert x xs)
                          True => x :: ys

insSort : Vect n Nat -> Vect n Nat
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                    insert x xsSorted
