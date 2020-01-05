--Exercise 2
--(a)
split :: (Ord a,Num a) => [a] -> ([a],[a])
split list = (filter positive list,filter negative list)
             where positive x = x>=0
                   negative x = x<0

--Exercise 4
--(a)
max1 :: Int -> Int -> Int
max1 x 0 = x
max1 x y| div x y==0 = y
        | otherwise = x

--other implementation
max2 :: Int -> Int -> Int
max2 x 0 = x
max2 0 x = x
max2 x y = 1 + max2 (x-1) (x-2)  

--(b)
maxList :: [Int] -> [Int] -> [Int]
maxList list1 list2 = [max x y|(x,y)<-zip list1 list2]

--Exercise 5
--(a)
unnamedFunction :: String -> [Int]
unnamedFunction str = map changeValue str

changeValue :: Char -> Int
changeValue x| x=='(' = 1
             | x ==')' = -1
             | otherwise = 0    

--(b)
unnamedFunction2 :: String -> Bool
unnamedFunction2 str = check (filter (/=0) (unnamedFunction str))

check :: [Int] -> Bool
check [x] = True
check (x:y:xs)|x>=y = check (y:xs)
              | otherwise = False

--Exercise 7 (filter but with duplicating)
duplicateSome :: (a -> Bool) -> [a] -> [a]
duplicateSome _ [] = []
duplicateSome f (x:xs)| f x = x:x:duplicateSome f xs
                      | otherwise = x:duplicateSome f xs

--Exercise 8
data Bit = O | I deriving Show
type BinNum = [Bit]

toBinNum :: Integer -> BinNum 
toBinNum 0 = []
toBinNum n|mod n 2==0 = O:toBinNum (div n 2)
          | otherwise = I:toBinNum (div n 2)

fromBinNum :: BinNum -> Integer
fromBinNum binary = fromBinNum1 binary 0

fromBinNum1 :: BinNum -> Integer -> Integer
fromBinNum1 [] _ = 0
fromBinNum1 (I:ns) n = 1*(2^n) + fromBinNum1 ns (n+1)
fromBinNum1 (O:ns) n = 0 + fromBinNum1 ns (n+1)


inc :: BinNum -> BinNum
inc x = toBinNum (fromBinNum x + 1)

inc1 :: BinNum -> BinNum
inc1 [] = [I]
inc1 (I:xs) = O:inc1 xs
inc1 (O:xs) = I:xs
        
