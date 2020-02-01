-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 19-21 Oct.

import Data.Char
import Test.QuickCheck
import Data.List


-- 1. Map
-- a.
uppers :: String -> String
uppers (xs) = (map toUpper[ x| x <- xs]) -- coverts a string to uppercase


-- b.
doubles :: [Int] -> [Int]
doubles xs = map double  [ x | x <- xs] -- doubles every item in a list
 where
double :: Int -> Int
double x = 2*x

-- c.        
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map penceToPoundsHelp [ x | x <- xs] -- turns pence into pounds

penceToPoundsHelp :: Int -> Float
penceToPoundsHelp x = (fromIntegral x/100)

--converts x which is an int into a float before applying it to the function. 

-- d.
uppers1 :: String -> String
uppers1 xs= [toUpper x | x <- xs]  -- coverts a string to uppercase

prop_uppers :: String -> Bool
prop_uppers xs = uppers xs == uppers1 xs


-- 2. Filter
-- a.
alphas :: String -> String
alphas xs  = filter isAlpha xs  -- removes non-alphas from string

-- b.
rmChar ::  Char -> String -> String
rmChar y xs = filter (/=y) xs    -- remove same character from a string                                                                                                                                                                                                                                                                         

-- how can you create a helper function ????

rmCharHelp :: Char -> Char -> Bool 
rmCharHelp y  (x)   | y==x = False
                       | otherwise = True

-- c.
above :: Int -> [Int] -> [Int] --removes all numbers greater or equal to in a list
above y xs  = filter ( >=y) xs

--aboveHelp :: Int-> Int -> Bool
--aboveHelp y x | x <= y = False
--              | otherwise = True 

-- how to create  a helper function ????

-- d.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter unequalHelp xs   --

unequalHelp :: (Int,Int) -> Bool
unequalHelp x | fst x == snd x = False -- checks wheather first element of a tuple equals to the second element of a tuple
              | otherwise = True
-- e.
rmCharComp :: Char -> String -> String
rmCharComp y xs  = [ x| x <- xs , x/=y] --removes character occurences

prop_rmChar :: Char -> String -> Bool
prop_rmChar y xs = rmChar y xs == rmCharComp y xs 



-- 3. Comprehensions vs. map & filter
-- a.
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]  

upperChars1 :: String -> String
upperChars1 s  = map toUpper (filter isAlpha s)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars1 s

-- b.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles1 :: [Int] -> [Int]
largeDoubles1 xs = map  (2 *) (filter ( >3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles1 xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven xs = [reverse s | s <- xs, even (length s)]

reverseEven1 :: [String] -> [String]
reverseEven1 (xs) = map reverse (filter evenHelp xs)

evenHelp :: String -> Bool
evenHelp (xs)   | even (length xs) = True   
                | otherwise = False

prop_reverseEven :: [String] -> Bool
prop_reverseEven xs = reverseEven xs == reverseEven1 xs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

productFold :: [Int] -> Int
productFold xs = foldr (*) 1  xs 

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

--when using foldr you need to give foldr the last element it needs to do the function too.

-- b.
andRec :: [Bool] -> Bool
andRec [] = True
andRec (x:xs) = x && andRec xs    

andFold :: [Bool] -> Bool
andFold xs =  foldr (&&) True xs

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.
concatRec :: [[a]] -> [a]
concatRec []=[]
concatRec (x:xs) = x ++ concatRec xs 

concatFold :: [[a]] -> [a]
concatFold xs = foldr( ++ ) [] xs

prop_concat :: [String] -> Bool
prop_concat xs = concatRec xs == concatFold xs

-- d.
rmCharsRec :: String -> String -> String
rmCharsRec [] xs = xs
rmCharsRec (c:cs) xs = rmCharsRec cs (rmChar c xs)  

rmCharsFold :: String -> String -> String
rmCharsFold [] xs = xs
rmCharsFold cs xs = foldr (rmChar) xs cs 

-- why does cs xs not work???????????????

prop_rmChars :: String -> String -> Bool
prop_rmChars cs xs = rmCharsRec cs xs == rmCharsFold cs xs


type Matrix = [[Int]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform (x:xs) = all (==x) xs

-- b.
valid :: Matrix -> Bool
valid xs = uniform ( map length xs) && ( length xs >=1) 
      
-- 6.

--a) uncurry is the inverse of curry. Its first arguement must be a function taking two values.Uncurry then applies the function on to the components of the pair which is the second arguement.

--b)

zipWith' :: (a -> b ->c) -> [a] -> [b] -> [c]
zipWith' f (xs) (ys) =  [ f x y | (x,y) <- zip xs ys] 

-- verison of how to write zipWith

--c)

zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f (xs) (ys) =  map  (uncurry f) (zip xs ys) 

-- verison of how to write zipWith

-- 7.
plusM :: Matrix -> Matrix -> Matrix
plusM  (xs) (ys) | (valid xs == valid ys) = zipWith plusHelper xs ys
                 | otherwise = error"Input not suitable"

plusHelper :: [Int] -> [Int] -> [Int]
plusHelper (xs) (ys) = zipWith (+) xs ys

-- 8.
--timesM :: Matrix -> Matrix -> [Int] --Matrix
-- timesM [] [] = []
--timesM xs ys   | (length (head (xs)) == length (head (transpose (ys)))) = zipWith dot xs (transpose ys) -- : timesM xs ys
               -- | otherwise = error "Input not suitable"

-- is there a way to do it by recursion????

timesM :: Matrix -> Matrix -> Matrix
timesM xs ys =[[dot x y | y <- transpose ys] | x <-xs,(length (transpose (xs)) == length (head  (ys)))]

-- without the error message.

timesM1 :: Matrix -> Matrix -> Matrix
timesM1 xs ys =[ if (length (transpose (xs)) == length (head  (ys))) then [dot x y | y <- transpose ys] else error "Input not suitable" | x <- xs]

-- is there a way to do this without the if statement in the list comprehension??
-- Gives error message 


dot ::[Int] -> [Int] -> Int
dot xs ys = sum (zipWith (*) xs ys)  

-- helper function which does the dot product of the row and column of a matrix                                               


--dotHelper ::Matrix -> Matrix -> Matrix
--dotHelper [] ys = []
--dotHelper xs [] = []
--dotHelper xs ys = [dot x (transpose(ys)) | x <- xs, ]      --(dot x y : dotHelper xs ys) 


-- Optional material
-- 9.
