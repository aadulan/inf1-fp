-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Due: the tutorial of week 3 (5-7th Oct.)

import Data.Char
import Data.List
import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [ (x `div` 2) | x <-xs , even x]

--This function takes the even numbers from a list xs and halves them.
--Example halveEvens [1,2,3,4,5,6] ouputs [1,2,3]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec []                   = []
halveEvensRec (x:xs)   | even x    = x `div` 2 : halveEvensRec xs
                       | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens x  =  halveEvens x  == halveEvensRec x

-- quickCheck (prop_halveEvens) ouputs +++ OK, passes 100 tests.



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x| x <- xs, x>=lo && x<= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi [] =[]
inRangeRec lo hi (x:xs)  | ( x>= lo && x<= hi) = x: inRangeRec lo hi xs
                         | otherwise =  inRangeRec lo hi xs
-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs

--quickCheck prop_inRange ouputs +++ OK, passes 100 tests.
-- why does quickCheck not work without inRangeRec to the list


-- 3. countPositives: count the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length[ x:xs | x <- xs , x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs) | (x > 0)   = 1+  countPositivesRec xs 
                         | otherwise = countPositivesRec xs
 

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs

--quickCheck prop_countPositivesRec ouputs +++ Ok, passed 100 tests.

-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x  = ( x - ( x `div` 10))

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum[ discount x | x <- xs,discount x<=19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) |(discount x <= 19900)= discount x + pennypincherRec xs 
                       | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs == pennypincherRec xs

-- quickCheck prop_pennypincher outputs +++ OK, passed 100 tests.



-- 5. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x <-xs , isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
                     | otherwise = multDigitsRec xs 
--why does it not work when 2nd line ends with 0?

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs
-- quickCheck prop_multDigtits ouputs with +++ Ok, passes 100 tests.                               



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise (x:xs) = (toUpper x) : [toLower y | y <- xs, isAlpha y]

-- How to give a guard for the first element???

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec "" = ""
capitaliseRec (x:xs)| isAlpha x = toUpper x : toLowerString xs 
                    
                    | otherwise = capitaliseRec xs      
                   

toLowerString :: String -> String 
toLowerString "" = ""
toLowerString (x:xs) |isAlpha x = toLower x : toLowerString xs
                     | otherwise = toLowerString xs 

--helper function which lowers the case for all the elements in the string



-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitalise xs == capitaliseRec xs 
              


-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title (x:xs) = ((capitalise x) : [ titleHelp y | y <- xs ])

titleHelp :: String -> String
titleHelp "" = ""
titleHelp (xs)   | (length (xs))   >= 4 =  capitalise xs
                 | otherwise            =  allLettersDown xs 
              -- | otherwise            = toLower(head(capitalise xs)) : tail(capitalise xs)

allLettersDown :: String -> String
allLettersDown "" = ""
allLettersDown (x:xs) = toLower x : allLettersDown xs 


--why would this function not work without brackets around capitalise x ?
-- without it, it does not work , starts to take words with less than 4 letters in it )

titleHelpRec :: [String] -> [String]
titleHelpRec [] = []
titleHelpRec (x:xs)  | length x >= 4 = capitalise(x) : titleHelpRec xs
                     | otherwise = (map toLower x)   : titleHelpRec xs


--REMEMBER WHAT TO DO IN A RECURSIVE FUNCTION !!!!!!!!!!!!!
-- Empty list = Empty List

 

-- Recursive version
titleRec :: [String] -> [String]
titleRec [] = []
titleRec (x:xs) = (capitaliseRec x):(titleHelpRec xs) 



                 

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind = undefined

-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec = undefined

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind = undefined 



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search = undefined

-- Recursive version
searchRec :: String -> Char -> [Int]
searchRec = undefined

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search = undefined


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains = undefined

-- Recursive version
containsRec :: String -> String -> Bool
containsRec = undefined

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains = undefined

