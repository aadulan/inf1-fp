-- Informatics 1 Functional Programming
-- December 2013
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> Int
f xs = sum [ (digitToInt x * 3^n) | (x,n) <- zip (reverse xs) [0..] ]

test1a = f "201"  == 19 &&
          f "12" == 5 &&
          f "1202" == 47 &&
          f "120221" == 430 

-- 1b

g :: String -> Int
g (x:xs) = (digitToInt x * 3^(n+1)) + g xs
          where n = head(reverse([0..(length xs)]))


test1b = f "201" == 19 &&
         f "12" == 5 &&
         f "1202" == 47 &&
         f "120221" == 430

-- Question 2

-- 2a

p :: [Int] -> Bool
p [] = error " Hi there , you have given me an empty list"
p (x:xs) =and [ y `mod` x == 0 |y <- xs , y>0 && x/=0]

test2a= p [2,6,-3,0,18,-17,10]  ==  True &&
        p [-13]                 ==  True &&
        p [-3,6,1,-3,9,18]      ==  False &&
        p [5,-2,-6,3]           == False 

-- 2b

q :: [Int] -> Bool
q [] = error "Hi there, you've given me an empty list"
q (y:[]) = True
q (x:y:xs) | y>0 && y `mod` x == 0 = True && q(x:xs)
           | y <= 0                = q(x:xs)
           | otherwise = False 

test2b = q [2,6,-3,0,18,-17,10]  ==  True &&
         q [-13]                 ==  True &&
         q [-3,6,1,-3,9,18]      ==  False &&
         q [5,-2,-6,3]           ==  False

-- 2c

r :: [Int] -> Bool
r (x:xs) =foldr (&&) True (map (\c->c`mod`x==0) (filter (>=0) xs ))
 

test2c = r [2,6,-3,0,18,-17,10]  ==  True &&
         r [-13]                 ==  True &&
         r [-3,6,1,-3,9,18]      ==  False &&
         r [5,-2,-6,3]           == False


-- Question 3

data Expr = X
          | Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (Neg p)    =  "(-" ++ showExpr p ++ ")"
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"

-- evaluate an Expr, given a value of X

evalExpr :: Expr -> Int -> Int
evalExpr X v          =  v
evalExpr (Const n) _  =  n
evalExpr (Neg p) v    =  - (evalExpr p v)
evalExpr (p :+: q) v  =  (evalExpr p v) + (evalExpr q v)
evalExpr (p :*: q) v  =  (evalExpr p v) * (evalExpr q v)

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM Neg subform
                                       , liftM2 (:+:) subform subform
                                       , liftM2 (:*:) subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- 3a

rpn :: Expr -> [String]
rpn X = ["X"]
rpn (Const x) = [show x]
rpn (Neg x)   = rpn x ++ ["-"]
rpn (p :+: q) =  rpn p ++ rpn q ++["+"]
rpn (p :*: q) =  rpn p ++ rpn q ++ ["*"]

-- 3 b

evalrpn :: [String] -> Int -> Int
evalrpn (X:y:xs)z = evalrpn (z:y:xs)
evalrpn (x:y : "-": xs) z = (x-y) (evalrpn xs z)
evalrpn (x:y:"+": xs)z = (x+y) (evalrpn xs z)
evalrpn (x:y:"*": xs)z = (x*y) (evalrpn xs z)
