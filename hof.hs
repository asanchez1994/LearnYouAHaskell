-- Higher Order Functions chapter from LYAH

compareWithHundred :: (Ord a, Num a) => a -> Ordering
compareWithHundred = compare 100 

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x) 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

makeTuple :: a -> b -> (a, b)
makeTuple x y = (x, y)

flip' :: (t2 -> t1 -> t) -> t1 -> t2 -> t
flip' f = g
    where g x y = f y x

map' :: (a -> b) -> [a] -> [b] 
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (t -> Bool) -> [t] -> [t]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x:filter' f xs
    | otherwise = filter' f xs 


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 479 == 0

-- sum of all odd squares less than 10,000
wayOne = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

wayTwo = sum (takeWhile (<10000) [x^2 | x <- [1..], odd (x^2)])

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n  = n:chain (n*3 + 1)

numLongChains :: Int 
numLongChains = length  (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- List of funs map with function of two parameters
listOfFuns = (*) [1..10]

-- numLongChains with lambda
numLongChains :: INt
numLongChains length (filter (\xs -> length xs > 15) (map chain [1..100]))
