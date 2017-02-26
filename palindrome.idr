module Demo

palindrome : String -> Bool
palindrome xs = (toLower xs) == (reverse (toLower xs))

test_palindrome : String
test_palindrome =  if palindrome "pullup"
                     && palindrome "Kayak"
                     && palindrome "noON"
                     && palindrome "Glenelg"
                     && palindrome "tattArratTat"
                     && palindrome "kuulilennuteetunneliluuk"
                     && not (palindrome "Idris")
                   then "Tests passed"
                   else "Tests failed"

counts : String -> (Nat, Nat)
counts xs = (length (words xs), length xs)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length n l = length (filter (\x => (length x) > n) l)
