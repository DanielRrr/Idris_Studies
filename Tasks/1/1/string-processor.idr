module Processor

import Data.List
import Data.String

palindrome : String -> Bool
palindrome xs = (toLower xs) == (reverse (toLower xs))

counts : String -> (Nat, Nat)
counts xs = (length (words xs), length xs)

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

over_length : Nat -> List String -> Nat
over_length n l = length (filter (\x => (length x) > n) l)

main : IO ()
main = do
	putStrLn "This is unimplemented interactive string processor"
	str <- getLine
	if head (words str) == "P"
		then if (palindrome (unwords (take 1 (tail (words str)))))
			then putStrLn "That's palindrome Amigo"
			else putStrLn "That's not palindrome Amigo"
		else putStrLn ("Result" ++ (show (counts (unwords (tail (words str)))))
