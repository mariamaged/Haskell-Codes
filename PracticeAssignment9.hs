--Exercise 9-1
--(a)
squareWithout :: Num a => [a] -> [a]
squareWithout [] = []
squareWithout (x:xs) = x^2:squareWithout xs
--(b)
squareWith :: Num a => [a] -> [a]
squareWith list = map (^2) list

--Exercise 9-2
--(a)
filtereven :: Integral a => [a] -> [a]
filtereven [] = []
filtereven (x:xs)|even x = x:filtereven xs
                 |otherwise = filtereven xs

--(b)
filter2 :: (a->Bool)-> [a] -> [a]
filter2 _ [] = []
filter2 f (x:xs)| f x = x:filter2 f xs
                | otherwise = filter2 f xs

 --(c)
filtereven2 :: Integral a => [a]-> [a]
filtereven2 list = filter2 even list

--Exercise 9-3
pairSummer :: Num a => (a,a)->a
pairSummer (x,y) = x+y

sumPairs :: Num a => [(a,a)]-> [a]
sumPairs list = map pairSummer list

--Exercise 9-4
deleteSpaces :: String -> String
deleteSpaces str = filter (/=' ') str

--Exercise 9-5
deepSumWith :: Num a => [[a]] -> a
deepSumWith list = sum (map sum list)

deepSumWithout :: Num a => [[a]] -> a
deepSumWithout [] = 0
deepSumWithout (x:xs) = sum x + deepSumWithout xs

--Exercise 9-6
--(a)
--[1,2,3,4]
--(b)
--take1 :: Integral a => a -> [b] -> [b]

--Exercise 9-7
zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 _ _ [] = []
zipWith1 _ [] _ = []
zipWith1 f (x:xs) (y:ys) = f x y:zipWith1 f xs ys

--Exercise 9-8
forsome :: (a->Bool) -> [a] -> Bool
forsome f list| length (filter f list)>0 = True
              | otherwise = False

--Exercise 9-9
forall :: (a -> Bool) -> [a] -> Bool
forall f list| length (filter f list) == length list = True
             | otherwise = False  


