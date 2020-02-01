-- Informatics 1 - Functional Programming 
-- Tutorial 2
--
-- Week 4 - due: 12-14 Oct.

import Data.Char
import Data.List
import Test.QuickCheck


-- 1.
--rotate :: Int -> [Char] -> [Char]
--rotate n xs = [drop n (x) | x <- xs, n>= 0 && n<= (length x)] : [take n (xs)| n >= 0 && n<= (length xs)]

rotate :: Int -> [Char] -> [Char]
rotate n (xs)   | n >= 0 && n <= length (xs) = drop n xs ++ take n xs
                | otherwise                  = error " n is negative or too large, try again "

-- ++ instead of : as here you are constructing two lists together while : takes an element to the list

-- 2.

prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- a) prop_rotate rotates the string input twice.It does this by first rotating the string by given my m. M is worked out if and only l does not equal 0. M is calculated by taking k which is an interger and working out the remainder of the division by l. The string is then rotated by  M and after this the string is rotated by the length of the string takeaway m.
-- b)prop_rotate aviods this error as l-m <= length of the string as l is the length of the string and m is always a postive number due to m being the remainder of the division.   


-- 3. 
makeKey :: Int -> [(Char, Char)]
makeKey n  = zip [ x | x <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"][ y| y <- rotate n "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]

-- need to take a seperate element out of the second list as then zip cannot make a pair. This is why you have to state what "y" is first.
-- could do ["A..Z"]

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp x xs = head ([ b |(a,b)<- xs , x ==a ]++ [x])


lookUpRec :: Char -> [(Char, Char)] -> Char
lookUpRec x [] = x
lookUpRec x (y:ys) | x == fst y = snd y 
                   | otherwise = lookUpRec x ys 



prop_lookUp :: Char -> [(Char, Char)] -> Bool
prop_lookUp x xs = lookUp x xs == lookUpRec x xs 

-- 5.
encipher :: Int -> Char -> Char
encipher x y = lookUp y (makeKey x) 

-- 6.
normalize :: String -> String
normalize (x:xs) = (toUpper x):[toUpper y | y <- xs , isAlpha y || isDigit y]

-- 7.
encipherStr :: Int -> String -> String
encipherStr x xs = normalize [encipher x (y) | y<-xs]
 

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs =  [(y,x) | (x,y) <- xs]

reverseKeyRec :: [(Char, Char)] -> [(Char, Char)]
reverseKeyRec [] = []
reverseKeyRec (x:xs) |x==(fst x, snd x)  = (snd x, fst x) : reverseKeyRec xs

prop_reverseKey :: [(Char, Char)] -> Bool
prop_reverseKey xs = reverseKey xs == reverseKeyRec xs 

-- 9.
decipher :: Int -> Char -> Char
decipher x y = lookUp y ( reverseKey( makeKey x))

decipherStr :: Int -> String -> String
decipherStr x xs = normalize [ decipher x (y) | y <- xs] 

-- 10.
contains :: String -> String -> Bool
contains b (l)     | (isInfixOf l b == True ) = True
                   | otherwise = False 

-- 11.
candidates :: String -> [(Int, String)]
candidates "" = []
candidates xs = [(x,y)| (x,y) <- (zip [1..26][decipherStr h xs | h <- [1..26]]), contains xs "THE" || contains xs "AND"]






-- Optional Material

-- 12.
--splitEachFive :: String -> [String]
--splitEachFive = undefined

-- 13.
--prop_transpose :: String -> Bool
--prop_transpose = undefined

-- 14.
--encrypt :: Int -> String -> String
--encrypt = undefined

-- 15.
--decrypt :: Int -> String -> String
--decrypt = undefined

-- Challenge (Optional)

-- 16.
--countFreqs :: String -> [(Char, Int)]
--countFreqs = undefined

-- 17
--freqDecipher :: String -> [String]
--freqDecipher = undefined
