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
halveEvens xs = [ x `div` 2 | x<- xs , x `mod` 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec []=[]
halveEvensRec (x:xs) | x `mod` 2 == 0  = x `div` 2 : halveEvensRec xs
                     | otherwise = halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensRec xs



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [ x | x <- xs , x>= lo && x<= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]
inRangeRec lo hi []=[]
inRangeRec lo hi (x:xs) | x>= lo && x<=hi = x : inRangeRec lo hi xs
                        |otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = inRange lo hi xs == inRangeRec lo hi xs



-- 3. countPositives: count the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives xs = length[ x| x<- xs , x>0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec ( x:xs) | x>0 = 1+ countPositivesRec xs
                          | otherwise = countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives xs = countPositives xs == countPositivesRec xs



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = round((fromIntegral x)* 0.9)

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum[ discount x | x<- xs , discount x <= 19900]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs) | discount x <= 19900 = discount x +  pennypincherRec xs
                       | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincherRec xs == pennypincher xs



-- 5. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = product [ digitToInt x | x<- xs , isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec []= 1
multDigitsRec (x:xs) | isDigit x = digitToInt x * multDigitsRec xs
                     | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs == multDigitsRec xs



-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise "" = ""
capitalise (x:xs) = (toUpper x) : [ toLower y | y <- xs , isAlpha y]

-- Recursive version
capitaliseRec :: String -> String
capitaliseRec "" = ""
capitaliseRec (x:xs) = (toUpper x) : help xs

help :: String -> String
help "" = ""
help (x:xs) | isAlpha x = toLower x : help xs
            | otherwise = help xs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = capitalise xs == capitaliseRec xs



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title [] =[]
title (x:xs) = (capitalise x) : [ if length y >= 4 then capitalise y else help y | y <-xs]


help1 :: String -> String
help1 xs = [toLower x | x<-xs]

-- Recursive version
titleRec :: [String] -> [String]
titleRec []=[]
titleRec (x:xs) = capitaliseRec x : help2 xs 

help2:: [String] ->[String]
help2 [] = []
help2 (x:xs) | length x >=4 = capitaliseRec x : help2 xs
             | otherwise = help1 x: help2 xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = title xs == titleRec xs 




-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter inPosition len words =

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

