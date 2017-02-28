palindrome : String -> Bool
palindrome xs = (toLower xs) == (reverse (toLower xs))
{-
  Write a complete program which prompts for an input, calls
  the function `palindrome` and prints its output. Use `avemain.idr`
  from the second lecture as example.
-}



main : IO ()
main = do
       putStrLn "This is unimplemented palidrome checker"
       putStrLn "I would like to ask you. Could you give a string? I want to check it"
       str <- getLine
       if (palindrome str)
                then putStrLn "I am pleased to inform that your string is palindrome"
                else putStrLn "Much to my regret, your string is not palindrome"
