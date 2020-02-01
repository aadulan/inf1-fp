-- Informatics 1 Functional Programming
-- Final Exam #1 - 7 December 2009
--
-- You do not need to put your name in this file
-- This examination will be marked anonymously


import Data.Char
import Test.QuickCheck


-- Question 1

-- 1a

f :: String -> Bool
f xs = and[ isUpper x |x<- xs , x == 'A' || x== 'E' || x == 'I' || x== 'O' || x == 'U' || x =='a' || x == 'e' || x== 'i' || x == 'o' || x=='u']

test1a = f "ALL CAPS"    == True &&
         f "r3cURsI0n"   == True &&
         f []            == True &&
         f "normal text" == False

-- 1b

isVowel:: Char -> Bool
isVowel x =  x == 'A' || x== 'E' || x == 'I' || x== 'O' || x == 'U' || x =='a' || x == 'e' || x== 'i' || x == 'o' || x=='u'

g :: String -> Bool
g [] = True
g (x:xs) | isVowel x = isUpper x && g xs
         | otherwise = g xs 

test1b = g "ALL CAPS"    == True &&
         g "r3cURsI0n"   == True &&
         g []            == True &&
         g "normal text" == False

-- 1c

h :: String -> Bool
h xs = foldr (&&) True (map isUpper (filter isVowel xs))

test1c = h "ALL CAPS"    == True &&
         h "r3cURsI0n"   == True &&
         h []            == True &&
         h "normal text" == False


prop1 xs = f xs == g xs && g xs == h xs 

check1 = quickCheck prop1 

-- Question 2

-- 2a

p :: [a] -> [a] -> [a]
p  xs ys = concat [[x,y]|(x,y)<-zip xs ys ] ++ drop (length xs) ys ++ drop (length ys) xs

test2a = p "itrev" "nelae" == "interleave" &&
         p "" "justalist"  == "justalist" &&
         p [0,0,0,0,0,0] [1,2,3] == [0,1,0,2,0,3,0,0,0]

-- 2b

q :: [a] -> [a] -> [a]
q [] ys = ys
q xs [] = xs
q (x:xs)(y:ys) = x:y : q xs ys 

test2b= q "itrev" "nelae" == "interleave" &&
        q "" "justalist"  == "justalist" &&
        q [0,0,0,0,0,0] [1,2,3] == [0,1,0,2,0,3,0,0,0]

-- Question 3

type Point = (Int,Int)
data Points = Rectangle Point Point
            | Union Points Points
            | Difference Points Points

-- 3a

inPoints :: Point -> Points -> Bool
inPoints = undefined

-- 3b

showPoints :: Point -> Points -> [String]
showPoints = undefined
