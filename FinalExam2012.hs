--Exercise 3
--(a)
bubble :: Ord a => [a] -> (a,[a])
bubble (v:vs) = (min1 v vs,delete (min1 v vs) (v:vs))

min1 :: Ord a => a -> [a] -> a
min1 min [] = min
min1 min (x:xs)| x<=min = min1 x xs
               | otherwise = min1 min xs

delete :: Ord a => a -> [a] -> [a]
delete _ [] = []
delete min (x:xs)|min==x = xs
                 |otherwise = x:delete min xs 
                 
--the solutions's solution (which is better :( )
bubble1 :: Ord a => [a] -> (a,[a])
bubble1 [x] = (x,[])
bubble1 (x:xs)|m<=x = (m,x:ys)    
              |otherwise = (x,xs)  
              where (m,ys) = bubble xs
              
--Exercise 4
--(a)
--taken from the solution because i couldn't solve it myself
sublist :: Eq a => [a] -> [a] -> Bool
sublist _ [] = False
sublist [] _ = True
sublist (x:xs) (y:ys) = (x==y && sublist xs ys) || (sublist (x:xs) ys)

--Exercise 5
sumSquaresPos :: (Ord a,Num a)=> [a] -> a
sumSquaresPos list = foldr (+) 0 (map (^2) (filter (>0) list)) 
{-sumSquaresPos = foldr (+) 0 (map square (filter positive list))   
                  where square x = x*x
                        positive x = x>=0 
                    -}     

--Exercise 7
--(a)
data Instruction = PUSH Int | ADD | SUB | MUL deriving Show
type Stack = [Int]
type Program = [Instruction]

--Not what is wanted
calc :: (Program,Stack) -> Stack
calc (program,stack)  = calc1 program stack

calc1 :: Program -> Stack -> Stack
calc1 [] _ = []
calc1 (ADD:ps) [s] = calc1 ps [s]
calc1 (SUB:ps) [s] = calc1 ps [s]
calc1 (MUL:ps) [s] = calc1 ps [s]
calc1 (PUSH p:ps) (s:ss) = calc1 ps (s:ss)++[p]
calc1 (ADD:ps) (s1:s2:ss) = calc1 ps ss ++ [s1+s2]
calc1 (SUB:ps) (s1:s2:ss) = calc1 ps ss ++ [s1-s2]
calc1 (MUL:ps) (s1:s2:ss) = calc1 ps ss ++ [s1*s2]

--correct implementation
calc2 :: (Program,Stack) -> Stack
calc2 ([],s) = s
calc2 (PUSH n:rest,s) = calc2 (rest,n:s)
calc2 (ADD:rest,s1:s2:s) = calc2 (rest,(s1+s2):s)
calc2 (SUB:rest,s1:s2:s) = calc2 (rest,(s1-s2):s)
calc2 (MUL:rest,s1:s2:s) = calc2 (rest,(s1*s2):s)


