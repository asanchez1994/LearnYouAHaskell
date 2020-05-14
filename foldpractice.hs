sum' :: (Num a) => [a] -> a
sum' = foldl (\acc x -> acc + x) 0 
