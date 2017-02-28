module Demo

import Data.Vect

insert : Ord a => (x : a) -> (xsSorted : Vect len a) -> Vect (S len) a
insert x [] = [x]
insert x ys@(y :: xs) = case x < y of
                          False => y :: (insert x xs)
                          True => x :: ys

insSort : Ord a => Vect n a -> Vect n a
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                    insert x xsSorted
