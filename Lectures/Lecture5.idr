module Demo

data NPair : Type where
  MkNPair : Nat -> Nat -> NPair

Show NPair where
  show (MkNPair n m) = "("++show n++","++show m++")"

sortAndShow : (Ord a, Show a) => List a -> String
sortAndShow xs = show (sort xs)

[myMaybe] Semigroup (Maybe a) where
  Nothing <+> m = m
  (Just x) <+>_ = Just x

[myCollectJust] Semigroup a => Semigroup (Maybe a) where
  Nothing   <+> m       = m
  m         <+> Nothing = m
  (Just m1) <+> (Just m2) = Just (m1 <+> m2)

m_add : Maybe Nat -> Maybe Nat -> Maybe Nat
m_add (Just n) (Just m) = Just (n + m)
m_add _ _ = Nothing

m_add' : Maybe Nat -> Maybe Nat -> Maybe Nat
m_add' a b = pure plus <*> a <*> b

m_add'' : Maybe Nat -> Maybe Nat -> Maybe Nat
m_add'' a b = [| a + b |]

m_add''' : Maybe Nat -> Maybe Nat -> Maybe Nat
m_add''' a b = pure (!a + !b)

a_add : (Semigroup a, Applicative f) => f a -> f a -> f a
a_add a b = [| a <+> b |]
