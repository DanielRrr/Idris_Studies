import Data.Vect
import Data.Fin
-- Use implicit arguments to implement following function
vlength : Vect n a -> Nat
vlength {n = k} _ = k

-- sum of vectors (define recursively)
vadd : Num a => Vect n a -> Vect n a -> Vect n a
vadd {n = Z} _ _ = Nil
vadd (x :: xs) (y :: ys) = (x + y) :: vadd xs ys

-- reverse given vector
reverse' : Vect n a -> Vect n a
reverse' [] = []
reverse' (x :: xs) = (reverse' xs) + [x]

-- scalar product (use functions zipWith and sum)
vScalarProd : Num a => Vect n a -> Vect n a -> a
vScalarProd xs ys = sum (zipWith (*) xs ys)


-- replace all coordinates that are close to zero with zero
toAxis : (eps : Double) -> Vect n Double -> Vect n Double


-- increase vector dimension, adding 0 as the first coordinate
incDimFirst : Vect n Double -> Vect (n+1) Double
incDimFirst xs = 0 :: xs

-- increase vector dimension, adding 0 as the last coordinate
incDimLast : Vect n Double -> Vect (n+1) Double
incDimLast xs = reverse (0 :: xs)

-- project vector to the space given by vector of indices
-- use function `map`
project : Vect n a -> Vect k (Fin n) -> Vect k a


test1 : Bool
test1 = project [1,2,3,4] [FS FZ, FZ, FS (FS FZ)] == [2,1,3]
        && project [1,2,3,4] [FS FZ, FS FZ, FS (FS (FS FZ))] == [2,2,4]
        && project [0] [FZ, FZ, FZ, FZ] == [0,0,0,0]
--      Following tests don't compile, why? Reasons differ!
--      && project [1,2,3,4] [FS FZ, FS FZ, (FS (FS (FS (FS FZ))))] == [2,2,0]
--      && project [0] [FZ, FZ, FZ, FZ] == [0,0,0]



-- matrix transposition
transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)


-- matrix addition and multiplication
addMatrix : Num numType => Vect rows (Vect cols numType)
                           -> Vect rows (Vect cols numType)
                           -> Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = (zipWith (+) x y) :: addMatrix xs ys


multMatrix : Num numType => Vect n (Vect m numType)
                            -> Vect m (Vect p numType)
                            -> Vect n (Vect p numType)
