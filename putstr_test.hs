main = do 
    putStr` "Howdy "
    putStr` "Friend. "
    putStrLn "How are you?"

putStr` :: String -> IO ()
putStr` [] = return ()
putStr` (x:xs) = do
    putChar x
    putStr` xs
