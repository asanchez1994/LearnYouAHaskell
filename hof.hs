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
    
numLongChains = length  (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- numLongChains with lambda
numLongChains' =  length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- foldl
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- no need to include second argument because foldl will return a function that takes on arg

sumElegant :: (Foldable t, Num a) => t a -> a
sumElegant = foldl (+) 0 

elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

mapFold' f xs = foldr (\x acc -> f x : acc) [] xs

-- implementations of standard functions with folds


maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc) 

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a 
product' = foldr1 (\x acc -> x * acc) 
-- or: product' = foldr1 (*) 

filterFold' :: (a -> Bool) -> [a] -> [a]
filterFold' f = foldr(\x acc -> if f x then x : acc else acc) []

sqrtSums = length (takeWhile(<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- $ function application, but with lowest precedence 
($) :: (a -> b) -> a -> b 
f $ x = f x 
-- f (g (z x))) = f $ g $ z x

-- function composition
mapExampleLam' = map (\x -> negate (abs x)) [1..15]
mapExampleComp' = map (negate . abs) [1..15]

