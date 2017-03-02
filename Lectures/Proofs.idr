import Data.Vect

data EqNat : (a : Nat) -> (b : Nat) -> Type where
  Same : (x : Nat) -> EqNat x x

checkEqNat : (a : Nat) -> (b : Nat) -> Maybe (EqNat a b)
checkEqNat Z Z = Just (Same 0)
checkEqNat Z (S k) = Nothing
checkEqNat (S k) Z = Nothing
checkEqNat (S k) (S j) =
    case checkEqNat k j of
      Nothing => Nothing
      Just (Same j) => Just (Same (S j))

exactLength' : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength' {m} len input =
  case checkEqNat len m of
      Nothing => Nothing
      (Just (Same m)) => Just input

checkEqNat' : (a : Nat) -> (b : Nat) -> Maybe (a = b)
checkEqNat' Z Z = Just Refl
checkEqNat' Z (S k) = Nothing
checkEqNat' (S k) Z = Nothing
checkEqNat' (S k) (S j) =
  case checkEqNat' k j of
    Nothing => Nothing
    Just eq => Just (cong eq)

rotate : Vect n a -> Vect n a
rotate [] = []
rotate (x :: xs) = rotateProof (xs ++ [x])
  where
    rotateProof : Vect (len + 1) a -> Vect (S len) a
    rotateProof {len} xs = rewrite plusCommutative 1 len in xs

plus_commutes_Z : (n : Nat) -> plus Z n = plus n Z
plus_commutes_Z Z = Refl
plus_commutes_Z (S k) = rewrite plus_commutes_Z k in Refl

plus_commutes_S : (k : Nat) -> (n : Nat) -> S (plus n k) = plus n (S k)
plus_commutes_S k Z = Refl
plus_commutes_S k (S j) = rewrite plus_commutes_S k j in Refl

plus_commutes : (m, n : Nat) -> m + n = n + m
plus_commutes Z n = plus_commutes_Z n
plus_commutes (S k) n = rewrite plus_commutes k n in plus_commutes_S k n

prop : (n, k : Nat) -> n + S k = S (n+k)
prop n k = sym (plusSuccRightSucc n k)

zeroNotSucc : (0 = S k) -> Void
zeroNotSucc Refl impossible

succNotZero : (S k = 0) -> Void
succNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNat'' : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNat'' Z Z = Yes Refl
checkEqNat'' Z (S k) = No zeroNotSucc
checkEqNat'' (S k) Z = No succNotZero
checkEqNat'' (S k) (S j) =
            case checkEqNat'' k j of
              (Yes prf) => Yes (cong prf)
              (No contra) => No (noRec contra)

hasOne : Elem 1 [1,2,3]
hasOne = Here

hasFalse : Elem False [True, True, False]
hasFalse = There (There Here)

removeElem : DecEq a => (v : a) -> (xs : Vect (S n) a) -> Elem v xs -> Vect n a
removeElem v (v :: ys) Here = ys
removeElem {n = Z} _ _ (There _) impossible
removeElem {n = (S k)} v (y :: ys) (There later) = y :: removeElem v ys later

not_in_nil : Elem v [] -> Void
not_in_nil Here impossible
not_in_nil (There _) impossible

not_in_tail : (contra : (v = x) -> Void) -> (contra1 : Elem v xs -> Void) -> Elem v (x :: xs) -> Void
not_in_tail contra contra1 Here = contra Refl
not_in_tail contra contra1 (There later) = contra1 later
