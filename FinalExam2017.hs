--Exercise 4
--(a)
changeFirst :: (a -> Bool) -> a -> [a] -> [a]
changeFirst _ _ [] = []
changeFirst p val (x:xs)| p x = val:xs
                        | otherwise = x:changeFirst p val xs

--Exercise 5
--(a)  
divisibility :: (Int,Int) -> Int
divisibility (x,y)| mod x y == 0 = x
                  | otherwise   =  0                
f :: [Int] -> [Int] -> Int
f list1 list2 = foldr (+) 0 (map divisibility (zip list1 list2))

--(b)
g :: [Int] -> [Int] -> Int
g list1 list2 = g1 (zip list1 list2)

g1 :: [(Int,Int)] ->Int
g1 [] = 0
g1 ((x,y):xs)| mod x y ==0 = x + g1 xs
             | otherwise = 0 + g1 xs

--Exercise 7
type Number = Int
type Point = (Number,Number)
type Length = Number
data Shape = Pt Point | Circle Point Length | Rect Point Length Length deriving Show
type Figure = [Shape]
type BBox = (Point,Point)

width :: Shape -> Length
width (Pt n) = 0
width (Circle n l) = l*2
width (Rect n w l) = w

bbox :: Shape -> BBox
bbox (Pt n) = (n,n)
bbox (Circle (x,y) l) = ((x-l,y-l),(x+l,y+l))
bbox (Rect (x,y) w l) = ((x,y),(x+w,y+l))

minX :: Shape -> Number
minX (Pt (x,y)) = x
minX (Circle (x,y) r) = x-r
minX (Rect (x,y) w l) = x


--(b)
addPt :: Point -> Point -> Point
addPt (x,y) (x1,y1) = (x+x1,y+y1)

--(c)
move :: Shape -> Point -> Shape
move (Pt n) vector = Pt (addPt n vector)
move (Rect n w l) vector = Rect (addPt n vector) w l
move (Circle n r) vector = Circle (addPt n vector) r
