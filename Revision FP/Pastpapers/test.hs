-- Informatics 1 Functional Programming
-- August 2016

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f xs = sum [((digitToInt x) * (2^n)) |(x,n)<- zip(reverse xs)[0..(length xs)] , (digitToInt x ==1)]

--Need to zip the (x,n) because the function then applies the all n to x which is not what you want.

test1 = f "101" == 5 &&
        f "11" == 3 && 
        f "1101" == 13 &&
        f "110111" == 55 

-- 1b

g :: String -> Int
g xs =  help   (xs)
   where
        help   [] = 0
        help  (x:xs) | (digitToInt x) == 1 = (2^n) + help  xs
                      | otherwise           = help xs
                 where n = head(reverse[0..(length xs)])
 

test2 = g "101" == 5 &&
        g "11" == 3 && 
        g "1101" == 13 &&
        g "110111" == 55

-- Question 2

-- 2a

p :: [Int] -> Bool
p xs = and[ odd x  |x<-xs , x `mod` 3 ==0]

test3 = p [1,15,153,83,64,9] == True &&
        p [1,12,153,83,9] == False &&
        p [] == True &&
        p [2,151] == True

--first you check wheather the number is divsible by 3 and then the corresponding numbers are checked wheather they are odd, from this you get a list of booleans and using the and function you are able to tell wheather all numbers divisble by three is odd.

-- 2b

q :: [Int] -> Bool
q [] = True
q (x:xs) | (x `mod` 3 ==0 && odd x == True) = q xs
         | (x `mod` 3 /= 0) = q xs
         | (x `mod` 3 ==0 && odd x == False) = False

test4 = q [1,15,153,83,64,9] == True &&
        q [1,12,153,83,9] == False &&
        q [] == True &&
        q [2,151] == True

-- here you go through the list to see wheather the list is odd and divisble by three and if it is , it carries on to the next number. Then if it is not divisble by three, you go on to the next letter and if it is divisble by three and is not odd , then it returns false.

-- 2c

r :: [Int] -> Bool
r xs =  foldr (&&) True (map odd (filter (\x-> x `mod` 3 == 0) xs))

test5 = r [1,15,153,83,64,9] == True &&
        r [1,12,153,83,9] == False &&
        r [] == True &&
        r [2,151] == True
   
-- first you filter all the numbers that are divisible by three and you check wheather they are odd using map then using foldr you check wheather it is all true.

-- Question 3

data Prop = X
          | F
          | T
          | Not Prop
          | Prop :->: Prop
          deriving (Eq, Ord)

-- turns a Prop into a string approximating mathematical notation

showProp :: Prop -> String
showProp X           =  "X"
showProp F           =  "F"
showProp T           =  "T"
showProp (Not p)     =  "(~" ++ showProp p ++ ")"
showProp (p :->: q)  =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"

-- For QuickCheck

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:->:) subform subform
                                       ]
                 where
                   atom = oneof [elements [X,F,T]]
                   subform  =  prop (n `div` 2)


-- 3a

eval :: Prop -> Bool -> Bool
eval (Not (Not x) :->: Not y) False = True 
eval (Not (Not x) :->: Not y) True  = False 
eval (Not x :->: Not (Not y)) v     = v
eval (Not x :->: Not y) v           = v
eval (Not x) True                   = False
eval (Not x) False                  = True
eval T _                            = True
eval F _                            = False
eval x v                            = v






--eval (Not(Not x)) True           = True
--eval (Not (Not x)) False         = False
--eval (Not x) False               = True
--eval (Not x) True                = False 
--eval ((Not x) :->: (Not y)) True = eval (Not x) True :->: eval (Not y) True




test3a = eval (Not T) True                    ==  False &&
         eval (Not X) False                   ==  True  &&
         eval (Not X :->: Not (Not X)) True   ==  True  &&
         eval (Not X :->: Not (Not X)) False  ==  False &&
         eval (Not (Not X :->: F)) True       ==  False &&
         eval (Not (Not X :->: F)) False      ==  True 


-- 3 b

simplify :: Prop -> Prop
simplify X            = X
simplify F            = F
simplify T            = T
simplify (Not F)      = T 
simplify (Not T)      = F
simplify (Not X)      = Not X
simplify (Not(Not x)) = simplify x
simplify (Not x)      = simplify (Not (simplify x))
simplify (T :->:p)    = simplify p 
simplify (F :->:p)    = T
simplify (p :->:T)    = T
simplify (p :->:F)    =simplify (Not (simplify p))
simplify (p :->: q)   = simplify (simplify p :->: simplify q)


test3b = simplify (Not F)                       ==  T &&
          simplify (Not X :->: Not (X :->: T))   ==  X &&
          simplify (Not (Not X :->: Not T))      ==  Not X &&
          simplify (Not (F :->: Not (Not X)))    ==  F
