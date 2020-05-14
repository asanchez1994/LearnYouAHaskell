-- Recursion chapter from LYAH

maximum' [] = error "problem"
maximum' [x] = x
maximum' (x:xs) 
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

maximumElegant :: Ord a => [a] -> a
maximumElegant [] = error "problem"
maximumElegant [x] = x
maximumElegant (x:xs) = max x (maximumElegant xs)

replicate' :: (Ord t1, Num t1) => t1 -> t -> [t]
replicate' x y
    | x <= 0 = []
    | otherwise = y:replicate' (x-1) y

take' :: (Ord t1, Num t1) => t1 -> [t] -> [t]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b):zip' as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (b:bs)
    | a == b    = True
    | otherwise = elem' a bs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a >= x]
        largerSorted = quicksort [b | b <- xs, b < x]
    in  largerSorted  ++ [x] ++ smallerSorted
