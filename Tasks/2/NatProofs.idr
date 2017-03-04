plus_n_zero : (n : Nat) -> n + 0 = n
plus_n_zero Z = Refl
plus_n_zero (S n) = rewrite plus_n_zero n in Refl

plus_n_succ : (n, k : Nat) -> n + (S k) = S (n + k)
plus_n_succ n k = sym (plusSuccRightSucc n k)

plus_assoc : (a, b, c : Nat) -> a + (b + c) = (a + b) + c
plus_assoc Z m p = Refl
plus_assoc (S n) m p = rewrite plus_assoc n m p in Refl

succ_inj : (n, m : Nat) -> S n = S m -> n = m
succ_inj n _ Refl = Refl

plus_eq : (n, m, p : Nat) -> n + m = n + p -> m = p
plus_eq Z m p r = r
plus_eq (S n) m p r = let iH = plus_eq n m p in iH (succ_inj _ _ r)

plus_right_zero : (n, m : Nat) -> n + m = n -> m = 0
plus_right_zero Z m r = r
plus_right_zero (S n) m r = plus_right_zero n m (succ_inj _ _ r)

-- if x > y then y <= x
-- gt__lte : x `GT` y -> y `LTE` x

-- if not(x <= y) then (y <= x)
-- not_lte__lte : Not (x `LTE` y) -> y `LTE` x
