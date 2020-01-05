--Exercise 3
rotabc :: String -> String
rotabc str = map valueEvaluator str

valueEvaluator :: Char -> Char
valueEvaluator char| char == 'a' = 'b'
                   | char == 'b' = 'c'
                   | char == 'c' = 'a'
                   | otherwise = char

--Exercise 4
isDigit :: Char -> Bool
isDigit char = char>='0' && char<= '9'

separate :: [Char] -> ([Char],[Char])
separate str = foldr f ([],[]) str

f :: Char -> ([Char],[Char]) -> ([Char],[Char])
f char1 (d,l)|isDigit char1 = ([char1]++d,l)
             |otherwise = (d,[char1]++l)

--Exercise 5
data Genre = Nonfiction | Novel | Biography deriving (Eq,Show)
type Name = (String,String)
type Date = (Int,Int,Int)
data Book = ABook Genre Name String Date Int deriving Show


genre :: Book -> Genre
genre (ABook g _ _ _ _) = g

title :: Book -> String
title (ABook _ _ s _ _) = s

date :: Book -> Date
date (ABook _ _ _ d _) = d

pages :: Book -> Int
pages (ABook _ _ _ _ n) = n 

year :: Book -> Int
year (ABook _ _ _ (_,_,x) _) = x

breakingNews = ABook Novel ("Schaetzing", "Frank") "Breaking News" (06, 03, 2014) 976
snowden = ABook Nonfiction ("Harding", "Luke") "The Snowden Files" (06, 02, 2014) 346
futureShock = ABook Nonfiction ("Toffler", "Alvin") "Future Shock" (01, 06, 1984) 576

--(a)
publishedIn :: Int -> [Book] -> [Book]
publishedIn year listBooks = filter (publishedInFilterHelper year) listBooks

publishedInFilterHelper :: Int -> Book -> Bool
publishedInFilterHelper yearWanted book| yearWanted == year book = True
                                       | otherwise = False

--(b)
totalPages :: [Book] -> Int
totalPages listBooks = sum (map pages listBooks)

--(c)
titlesOf :: Genre -> [Book] -> [String]
titlesOf genre listBooks = map title (filter (genreHelper genre) listBooks)

genreHelper :: Genre -> Book -> Bool
genreHelper genreName book| genre book == genreName = True
                          | otherwise = False

