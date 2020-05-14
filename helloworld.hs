import Data.Char

-- main = putStrLn "hello, world"

main = do
    putStrLn "What is your first name?"
    firstName <- getLine
    putStrLn "What is your last name?"
    lastName <- getLine
    let bigFirst = map toUpper firstName
        bigLast = map toUpper lastName
    putStrLn $ "Hello, " ++ bigFirst ++ " " ++ bigLast ++ "."
