--Exercise 7-1
--(a)
data Tree = Nil | BT Int Tree Tree deriving Show
--(b-1)
sumTree :: Tree ->Int
sumTree Nil = 0
sumTree (BT n left right) = n + sumTree left + sumTree right
--(b-2)
double :: Tree->Tree
double Nil = Nil
double (BT n left right) = BT (n*2) (double left) (double right)
--(b-3)
maxElement :: Tree->Int
maxElement (BT n left right) = maxElement1 (maxElement1 n left) right

maxElement1 :: Int -> Tree-> Int
maxElement1 maxSoFar Nil = maxSoFar
maxElement1 maxSoFar (BT n left right) | n>maxSoFar = maxElement1 (maxElement1 n left) right
                                       | otherwise =  maxElement1 (maxElement1 maxSoFar left) right

--Exercise 7-2
--(a)
data Expr = N Int | Plus Expr Expr | Equal Expr Expr | Not Expr deriving Show
data Val =  I Int | B Bool deriving Show

eval :: Expr->Val
eval (N n) = I n
eval (Plus n1 n2) = I (i+j) where (I i, I j) = (eval n1,eval n2)
eval (Equal n1 n2) = B (i==j) where (I i,I j) = (eval n1,eval n2)
eval (Not n) = B (not i) where B i = eval n