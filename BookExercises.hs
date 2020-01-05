--(function1)
double1 x = x+x

--(function2)
sum1 :: Num a => [a]->a
sum1 [] = 0
sum1 (x:xs) = x + sum xs

--(function3)
qsort1 :: Ord a => [a]->[a]
qsort1 [] = []
qsort1 (x:xs) = qsort1 smaller ++ [x] ++ qsort1 larger
               where smaller = [a | a<-xs, a<=x]
                     larger = [a | a<-xs,a>x]

--(function 4)
quadraple1 x = double1 (double1 x)

--(function 5)
factorial n = product [1..n]

--(function 6)
average ns = div (sum ns) (length ns)

--(function 7--curried function example)
add' :: Int->Int->Int
add' x y = x+y

add'' :: Int->Int
add'' x = add' x 1

ad :: Int->Int->Int
ad 1 x = x+1
ad 2 x = x+2
{-
Some Types:
-----------
length:: [a]->Int
fst:: (a,b)->a
head:: [a]->a
take:: Int->[a]->[a]
zip::[a]->[b]->[(a,b)]
id:: a->a

-------------
(Eq class)
(==):: Eq a => a->a->Bool
(/=):: Eq a => a->a->Bool

(Ord class)
(<):: Ord a => a->a->Bool
(>):: Ord a => a ->a->Bool
(>=):: Ord a => a->a->Bool
(<=)::Ord a => a->a->Bool
min:: Ord a => a->a->a
max:: Ord a => a->a->a

(Show and Read class)
show:: Show a=> a->String
read:: Read a=> String->a

(Num class)
(+):: Num a => a->a->a
(-):: Num a => a->a->a
(*):: Num a => a->a->a
abs:: Num a => a->a
negate :: Num a => a->a
signum :: Num a => a->a

(Integral class)
div:: Integral a => a->a->a
mod:: Integral a => a->a->a

(Fractional class)
(/):: Fractional a => a->a->a
recip:: Fractional a => a->a
-}

{-
Synonyms of existing types:
--------------------------
type String = [Char]
type Board = [Pos]
type Pos = (Int,Int)
-}

{-
Completely new types:
--------------------
data Bool = True | False
data Move = Left | Right | Up | Down
data Shape = Circle Float | Rect Float Float
data Maybe a = Nothing | Just a
data Nat = Zero | Succ Nat
-}


{-
move :: Move->Pos->Pos
move Left (x,y) = (x-1,y)
move Right (x,y) = (x+1,y)
move Up (x,y) = (x,y+1)
move Down (x,y) = (x,y-1)

moves :: [Move]->Pos->Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

flip:: Move->Move
flip Right = Left
flip Left = Right
flip Up = Down
flip Down = Up

square::Float->Shape
square n = Rect n n

area::Shape->Float
area (Circle a) = pi*r^2
area (Rect a b) = a*b
-}

safediv :: Integral a => a->a->Maybe a
safediv m n| n==0 = Nothing 
           | otherwise = Just (div m n)

safehead:: [a]->Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

data Nat = Zero | Succ Nat

nat2int:: Nat-> Int
nat2int Zero = 0
nat2int (Succ n) = 1+ nat2int n

int2nat:: Int->Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addNat::Nat->Nat->Nat
addNat m n = int2nat (nat2int m+ nat2int n)

addNat1::Nat->Nat->Nat
addNat1 Zero n = n
addNat1 (Succ m) n = Succ (addNat1 m n)

data List a = Nil | Cons a (List a)

len :: List a->Int
len Nil = 0
len (Cons _ xs) = 1+ len xs


data Tree = Leaf Int | Node Int Tree Tree

occurs:: Int->Tree->Bool
occurs m (Leaf n) = m==n
occurs m (Node n l r) = m==n || occurs m l || occurs m r
{-
flatten :: Tree -> Int
flatten (Leaf n) = n
flatten (Node n l r) = flatten l ++ [n] ++ flatten r

-}
occurs1 :: Int->Tree->Bool
occurs1 m (Leaf n) = m==n
occurs1 m (Node n l r)| m==n = True
                      | m<n = occurs1 m r
                      | otherwise = occurs1 m l


data Expr = Val Int | Add Expr Expr

value:: Expr->Int
value (Val n) = n
value (Add n m) = value n + value m


--Higher Order Functions

twice :: (a->a)->a->a
twice f x = f (f x)

quadraple2 x = twice (*2) x

map1 :: (a->b)->[a]->[b]
map1 f xs = [f x | x<-xs]


filter1 :: (a->Bool)->[a]->[a]
filter1 p xs = [x | x<-xs ,p x]

sumsqreven :: [Int]->Int
sumsqreven ns = sum (map1 (^2) (filter1 even ns))

foldr2:: (a->b->b)->b->[a]->b
foldr2 f v [] = v
foldr2 f v (x:xs) = f x (foldr2 f v xs)

snoc x xs = xs ++ [x]
sum2 (x:xs) = foldr (+) 0 (x:xs)
reverse1 (x:xs) = foldr snoc [] (x:xs)




