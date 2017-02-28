even : Nat -> Bool
even Z = True
even (S n) = odd n
  where
    odd : Nat -> Bool
    odd Z = False
    odd (S n) = even n


-- Should be True
test1 : Bool
test1 = even 2 && even (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))

{-
Define recursive function, which is evaluated according to the following rules:
               |  0,     if a < b
     a .-. b = |
               |  a - b, otherwise
-}

infixl 10 .-.

(.-.) : Nat -> Nat -> Nat
(.-.) a Z = a
(.-.) Z a = Z
(.-.) (S x)(S y) = (.-.) x y

-- Should be True
test2 : Bool
test2 = (5 .-. 2 == 3) && (2 .-. 5 == 0)

abs' : Nat -> Nat -> Nat
abs' x y = ((.-.) x y) +  ((.-.) y x)

-- Should be True
test3 : Bool
test3 = abs' 2 5 == 3

plus'' : Nat -> Nat -> Nat
plus'' x Z = x
plus'' x (S y) = S (plus'' x y)

test4 : Bool
test4 = (plus'' 2 5 == 7) && (plus'' 5 2 == 7)

all_tests : Bool
all_tests = test1 && test2 && test3 && test4
