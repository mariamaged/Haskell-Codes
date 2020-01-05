--Exercise 8-1
--(a)
last1 :: [a]->a
last1 [x] = x
last1 (x:xs) = last1 xs

--(b)
occursIn :: Eq a => a -> [a] -> Bool
occursIn _ [] = False
occursIn x (y:ys)|x==y = True
                 |otherwise = occursIn x ys

--(c)
--one way using mapping
----------------------
occurs :: Eq a => a -> [[a]] -> [Bool]
occurs x y = map (occursIn x) y

occurs1 :: Eq a => a -> [[a]] -> [[a]]
occurs1 x y = occurs2 (occurs x y) 0  y

occurs2 :: Eq a => [Bool]->Int->[[a]]->[[a]]
occurs2 [] _ _ = []
occurs2 (b:bs) ind y| b = y!!ind:occurs2 bs (ind+1) y
                    | otherwise = occurs2 bs (ind+1) y
--recursion way
-----------------------
occursFinal :: Eq a => a -> [[a]] -> [[a]]
occursFinal a [] = []
occursFinal x (y:ys)| occursIn x y = y:occursFinal x ys
                     | otherwise = occursFinal x ys
 
 
--(d)
--using foldr 
reverse1 :: [a] -> [a]
reverse1 x = foldr sonc [] x

sonc :: a -> [a] -> [a]
sonc x y = y ++ [x]
 
--(e)
maxList :: Ord a => [a] -> a
maxList (x:xs) = maxList1 xs x 

maxList1 :: Ord a => [a] -> a -> a
maxList1 [] maxSoFar = maxSoFar
maxList1 (x:xs) maxSoFar| x>maxSoFar = maxList1 xs x
                        | otherwise =  maxList1 xs maxSoFar


--(f)
updatePrices :: Float -> [(String,Float)] -> [(String,Float)]
updatePrices percent cart = [(itemName,price+price*percent/100)|(itemName,price)<-cart]

--Exercise 8-2
palindrome :: String -> Bool
palindrome x| reverse x==x = True 
            | otherwise = False

--Exercise 8-3
prefix :: String -> String -> Bool
prefix str1 str2| take (length str1) str2==str1 = True
                | otherwise = False

--Exercise 8-4
--(a)
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)|x<y = x:y:ys
               |otherwise = y:insert x ys
               
--(b)
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)  

--Exercise 8-5
merge :: Ord a => [a] -> [a] -> [a]
merge x y = insertSort (x ++ y)