--Exercise 6-1
--(a)
square :: Num a => a->a
square x = x*x

--(b)
cube :: Num a => a->a
cube x = x*x*x

--(c)
cube1 :: Num a =>a->a
cube1 x = square x*x

--(d)
fourthpower :: Num a => a ->a
fourthpower x = square x * square x

--(e)
isTriple :: Num a => a -> a -> a -> Bool
isTriple x y z = if square x + square y == square z then True else False

--Exercise 6-2
--(a)
threeDifferent :: Eq a => a -> a -> a -> Bool
threeDifferent m n p = m/=n && m/=p && n/=p

threeDifferent1 :: Eq a => a -> a -> a -> Bool
threeDifferent1 m n p| m==n = False
                     | m==p = False
                     | n==p = False
                     | otherwise = True

--Exercise 6-3
--(a)
minThree :: Ord a => a -> a -> a -> a
minThree x y z = minThree1 [y,z] x 

minThree1 :: Ord a => [a] -> a -> a
minThree1 [] smSoFar = smSoFar
minThree1 (x:xs) smSoFar| x<=smSoFar = minThree1 xs x
                        | otherwise  = minThree1 xs smSoFar

--other different way using the predefined function
minThree2 :: Ord a => a -> a -> a -> a
minThree2 x y z = min (min x y) z

--(b)
--other different way using the explained function
minThree3 :: Ord a => a -> a -> a -> a
minThree3 x y z = smaller (smaller x y) z

smaller :: Ord a => a -> a -> a
smaller x y| x<=y = x
           | otherwise = y


--Exercise 6-4
--(a)
sumsq :: Int -> Int
sumsq n = sum (map square [1..n])

--(b)
fib :: Int->Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

--(c)
isPrime :: Int -> Bool
isPrime x = if length (factorCompiler x (x-1))==0 then True else False

factorCompiler :: Int->Int->[Int]
factorCompiler _ 1 = []
factorCompiler x y| mod x y == 0 = y:factorCompiler x (y-1)
                  | otherwise = factorCompiler x (y-1)

--(d)
primeGEQ :: Int -> Int
primeGEQ x|isPrime x = x
          |otherwise = primeGEQ (x+1)    
          
--(e)
gcd1 :: Int ->Int->Int
gcd1 x y = if x<y then gcd1 y x
          else if y==0 then x
          else gcd1 y (mod x y)   
          
--(f)
power :: Int -> Int ->Int
power _ 0 = 1
power x 1 = x
power a b| even b = power a (div b 2)*power a (div b 2)
         | otherwise = a*power a (b-1)      
         