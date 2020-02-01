-- Informatics 1 Functional Programming
-- December 2015
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

p :: [Int] -> Int
p ts = 1 + (((sum[t |t <-ts, t>=0]) `div` 60) `mod` 12 )

test1 = p [] == 1 && 
        p [-30,-20] == 1 &&
        p [20,-30, 30,14,-20] == 2 &&
        p [200,45] == 5 &&
        p [60,-100,360,-20,240,59] == 12 &&
        p [60,-100,360,-20,240,60] ==1
        

-- p takes the sum of the durations and then divides it by 60 to give the hour. It then does the modulus of it by 12 and plus 1. NOTE: +1 cannot be with the summation at then 12 mod 12 = 0 when 12 should be your answer.

-- 1b

q :: [Int] -> Int
q xs = 1 + ((help xs `div` 60) `mod` 12) 

help:: [Int] -> Int
help [] = 0  
help (x:xs) | x >= 0 = x+ help xs
            | otherwise = help xs  

test2 = q [] == 1 && 
        q [-30,-20] == 1 &&
        q [20,-30, 30,14,-20] == 2 &&
        q [200,45] == 5 &&
        q [60,-100,360,-20,240,59] == 12 &&
        q [60,-100,360,-20,240,60] == 1

-- 1c

r :: [Int] -> Int
r xs= 1+(((foldr (+) 0(filter (>= 0) xs)) `div` 60 ) `mod` 12)

test3 = r [] == 1 && 
        r [-30,-20] == 1 &&
        r [20,-30, 30,14,-20] == 2 &&
        r [200,45] == 5 &&
        r [60,-100,360,-20,240,59] == 12 &&
        r [60,-100,360,-20,240,60] == 1


-- First you filter the list of time durations to get the positive numbers and using foldr, you add the numbers together and the you divide by 60 to get hours with mod 12 and then you plus 1.

-- Question 2

-- 2a

f :: String -> String
f [] =[]
f (x:xs) = x :[ z | (y,z)<- zip(x:xs)(xs) , y/=z]

test4 = f "Tennessee" == "Tenese" &&
        f "llama"     == "lama" &&
        f "oooh"  == "oh" &&
        f "none here" == "none here" &&
        f "nNnor hEere" == "nNnor hEere" &&
        f "A" == "A" &&
        f "" == ""

-- Need to include a base case for the empty string. The reason why you give z in the list comprehension instead of y is because you want to give the second letter if it is only not the same as before.

-- 2b

g :: String -> String
g "" = ""
g (x:y:xs) |x /= y = x : y : g xs
           | otherwise = g (y:xs)

test5 = g "Tennessee" == "Tenese" &&
        g "llama"     == "lama" &&
        g "oooh"  == "oh" &&
        g "none here" == "none here" &&
        g "nNnor hEere" == "nNnor hEere" &&
        g "A" == "A" &&
        g "" == ""

--Why am I getting non exhaustive patterns????                   
      
-- Question 3

data Regexp = Epsilon
            | Lit Char
            | Seq Regexp Regexp
            | Or Regexp Regexp
        deriving (Eq, Ord)

-- turns a Regexp into a string approximating normal regular expression notation

showRegexp :: Regexp -> String
showRegexp Epsilon = "e"
showRegexp (Lit c) = [toUpper c]
showRegexp (Seq r1 r2) = "(" ++ showRegexp r1 ++ showRegexp r2 ++ ")"
showRegexp (Or r1 r2) = "(" ++ showRegexp r1 ++ "|" ++ showRegexp r2 ++ ")"

-- for checking equality of languages

equal :: Ord a => [a] -> [a] -> Bool
equal xs ys = sort xs == sort ys

-- For QuickCheck

instance Show Regexp where
    show  =  showRegexp

instance Arbitrary Regexp where
  arbitrary = sized expr
    where
      expr n | n <= 0 = oneof [elements [Epsilon]]
             | otherwise = oneof [ liftM Lit arbitrary
                                 , liftM2 Seq subform subform
                                 , liftM2 Or subform subform
                                 ]
             where
               subform = expr (n `div` 2)



r1 = Seq (Lit 'A') (Or (Lit 'A') (Lit 'A'))   -- A(A|A)
r2 = Seq (Or (Lit 'A') Epsilon)
         (Or (Lit 'A') (Lit 'B'))             -- (A|e)(A|B)
r3 = Seq (Or (Lit 'A') (Seq Epsilon
                            (Lit 'A')))
         (Or (Lit 'A') (Lit 'B'))             -- (A|(eA))(A|B)
r4 = Seq (Or (Lit 'A')
             (Seq Epsilon (Lit 'A')))
         (Seq (Or (Lit 'A') (Lit 'B'))
              Epsilon)                        -- (A|(eA))((A|B)e)
r5 = Seq (Seq (Or (Lit 'A')
                  (Seq Epsilon (Lit 'A')))
              (Or Epsilon (Lit 'B')))
         (Seq (Or (Lit 'A') (Lit 'B'))
              Epsilon)                        -- ((A|(eA))(e|B))((A|B)e)
r6 = Seq (Seq Epsilon Epsilon)
         (Or Epsilon Epsilon)                 -- (ee)(e|e)

-- 3a

language :: Regexp -> [String]
language Epsilon     = [""]
language (Lit x)       = [[x]]
language (Seq xs ys)   = nub[ x++y | x <- language xs , y <-language ys]
language (Or x y)      = nub(language x ++ language y )

test6 = language r1 == ["AA"] &&
        language r2 == ["AA","AB","A","B"] &&
        language r3 == ["AA","AB"] &&
        language r4 == ["AA","AB"] &&
        language r5 == ["AA","AB","ABA","ABB"] &&
        language r6 == [""] 
-- 3b

simplify :: Regexp -> Regexp
simplify (Seq exp1 exp2) | simplify exp1 == Epsilon = simplify exp2
                         | simplify exp2 == Epsilon = simplify exp1
                         | otherwise = Seq (simplify exp1)(simplify exp2)
simplify(Or exp1 exp2)   | simplify exp1 == simplify exp2 = simplify exp1
                         | simplify exp2 == simplify exp1 = simplify exp2
                         | otherwise = Or (simplify exp1)(simplify exp2)
simplify exp = exp

test3b= simplify r1 == Seq (Lit 'A') (Lit 'A') &&
        simplify r2 == r2 &&
        simplify r3 == Seq (Lit 'A') (Or (Lit 'A') (Lit 'B')) &&
        simplify r4 == Seq (Lit 'A') (Or (Lit 'A') (Lit 'B')) &&
        simplify r5 == Seq (Seq (Lit 'A')
                                (Or Epsilon (Lit 'B'))) 
                                             (Or (Lit 'A') (Lit 'B'))               &&
        simplify r6 == Epsilon   
