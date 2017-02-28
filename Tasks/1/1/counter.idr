
{-
  Write a complete program which prompts for an input, calls
  the function `counts` and prints its output.
-}

counts : String -> (Nat, Nat)
counts xs = (length (words xs), length xs)


main : IO ()
main = do
        putStrLn "This is unimplemented words and characters counter"
        putStrLn "Give me a String"
        str <- getLine
        putStrLn ("Counts: " ++ (show (counts str)))
