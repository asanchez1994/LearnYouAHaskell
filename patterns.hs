-- Syntax in Functions chapter from LYAH

lucky :: (Eq a, Num a) => a -> [Char]
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck pal!"

fact :: (Eq t, Num t) => t -> t
fact 0 = 1
fact x = x * fact (x-1)

addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

secondExtract (_, y, _) = y

head' :: [a] -> a
head' [] = error "Problems"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs


sum' :: (Num b) => [b] -> b 
sum' [] = 0 
sum' all@(x:xs) = x + sum' xs 

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 10.0 = "Thin"
    | bmi <= 20.0 = "Fat"
    | otherwise = "Something else"
    where bmi = weight / height ^ 2 

max' a b
    | a > b = a
    | otherwise = b 

initialsPrint :: String -> String -> String
initialsPrint (a:_) (b:_) = [a] ++  "." ++ [b] ++ "."

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi h w | (h, w) <- xs]
    where bmi w h = w / h ^ 2 

cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

cylinderWhere r h = sideArea + 2 * topArea
    where sideArea = 2 * pi * r * h
          topArea = pi * r^2
 

    


