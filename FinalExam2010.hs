--Exercise 3
--(a)
snoc :: Int -> [Int] -> [Int]
snoc x list = list ++ [x]

--(b)
reverseFiltering :: [Int] -> [Int]
reverseFiltering list = foldr snoc [] list

--(c) 
{-
f :: [[Int]] -> [[Int]]
f x = map reverseFiltering x
-}

--Exercise 4 
span1 :: (a->Bool) -> [a] -> ([a],[a])
span1 p [] = ([],[])
span1 p (x:xs)|p x = (x:ys,zs)
              |otherwise = ([],(x:xs))
              where (ys,zs) = span1 p xs

--Exercise 5
break1 :: (a -> Bool) -> [a] -> ([a],[a])
break1 _ [] = ([],[])
break1 p (x:xs)|p x == False = (x:ys,zs)
               |otherwise = ([],(x:xs))
               where (ys,zs) = break1 p xs


--Exercise 6
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

swapTree :: Tree a -> Tree a
swapTree Leaf = Leaf
swapTree (Node left n right) = Node (swapTree right) n (swapTree left)

--(b) bonus question
paths1 :: Tree a -> [[a]]
paths1 tree = p [] tree 
              where p path Leaf = []
                    p path (Node Leaf x Leaf) = [path++[x]]
                    p path (Node left x right) = p (path++[x]) left ++ p (path++[x]) right


--Exercise 7
data Polynomial a = Const a | Var String | Sum (Polynomial a) (Polynomial a)| Product (Polynomial a) (Polynomial a)  

multPol :: Polynomial a -> Polynomial a -> Polynomial a
multPol (Sum x y) z = Sum (multPol x z) (multPol y z)
multPol x y = Product x y
