--Exercise 3
--(a)
isFaceCard :: Char -> Bool
isFaceCard card| card=='A' || card =='J' || card == 'K' || card == 'Q' = True
               | otherwise = False

--(b)
isCard :: Char -> Bool
isCard card| card>='2' && card<='9' = True
           | card == '0' = True
           | isFaceCard card = True
           | otherwise = False
--(c)
f :: String -> Bool
f [] = True
f (c:cs)| isCard c = isFaceCard c && f cs
        | otherwise = f cs

--(d) using filtering twice       
g :: String -> Bool
g cardString| length (filter isFaceCard (filter isCard cardString)) == length (filter isCard cardString) = True
            | otherwise = False          

 --(e) using mapping and filtering and foldr           
h :: String -> Bool
h cardString = foldr (&&) True (map isFaceCard (filter isCard cardString))

--Exercise 4
--(a)
f1 :: [Int] -> Bool
f1 [x] = True
f1 (x:y:xs) | y>=(x*2) = f1 (y:xs)
            | otherwise = False

--Exercise 5
cube :: Int -> Int
cube x = x*x*x
p :: [Int] -> Int
p list = sum (map cube (filter (>0) list))
-- or we can use foldr p list = foldr (+) 0 (map cube (filter (>0) list))

--Exercise 6
data Proposition = Var String | F | T | Not Proposition | Proposition :|: Proposition | Proposition :&: Proposition deriving Show
--(a)
isNorm :: Proposition -> Bool
isNorm (Var n) = True
isNorm T = True
isNorm F = True   
isNorm (Not (Var _)) = True
isNorm (Not _ ) = False
isNorm ((:&:) p1 p2) = isNorm p1 && isNorm p2   
isNorm ((:|:) p1 p2) = isNorm p1 && isNorm p2
 
--(b)
norm :: Proposition -> Proposition
norm (Var n) = Var n
norm T = T
norm F = F
norm (Not (Var n)) = Not (Var n)
norm (Not T) = F
norm (Not F) = T
norm (Not (Not n)) = norm n
norm (Not ((:&:) p1 p2)) = (:|:) (norm (Not p1)) (norm (Not p2))
norm (Not ((:|:) p1 p2)) = (:&:) (norm (Not p1)) (Not (Not p2))
norm ((:|:) p1 p2) = (:|:) (norm p1) (norm p2)
norm ((:&:) p1 p2) = (:&:) (norm p1) (norm p2)



