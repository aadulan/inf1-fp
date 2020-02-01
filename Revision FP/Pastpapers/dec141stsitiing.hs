-- Informatics 1 Functional Programming
-- December 2014
-- SITTING 1 (09:30 - 11:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM2, liftM3, used below
import Data.Char

-- Question 1

-- 1a

f :: [Int] -> Bool
f [] = error "Hi, there you have given me an empty list"
f xs = and [ y `mod` x == 0 |(x,y) <- zip (xs)(drop 1 xs)]

test1a = f [1,1,-2,6,18,-18,180] == True &&
         f [17] == True  &&
         f [1,1,2,3,6,18] == False &&
         f [1,2,6,3,9] == False 

-- 1b

g :: [Int] -> Bool
g [] = error " Hi there, you've given me a empty list"
g [x] = True
g (x:y:xs) | y `mod` x == 0 = g(y:xs)
           | otherwise = False 

test1b = g [1,1,-2,6,18,-18,180] == True &&
         g [17] == True  &&
         g [1,1,2,3,6,18] == False &&
         g [1,2,6,3,9] == False 

test1 = test1a == test1b


-- Question 2

-- 2a

p :: [Int] -> Int
p xs = (product[ x |x<- xs , x<0])^2

test2a = p [13] == 1 &&
         p [] == 1 &&
         p [-3,3,1,-3,2,-1] == 81 &&
         p [2,6,-3,0,3,-7,2] == 441 &&
         p [4,-2,-1,-3] == 36 

-- 2b

q :: [Int] -> Int

q  xs = (help xs) ^2
  where help [] = 1
        help (x:xs) | x<0 = x* help xs
                    | otherwise = help xs

test2b =  q [13] == 1 &&
          q [] == 1 &&
          q [-3,3,1,-3,2,-1] == 81 &&
          q [2,6,-3,0,3,-7,2] == 441 &&
          q [4,-2,-1,-3] == 36 



-- 2c

r :: [Int] -> Int
r  xs = (foldr (*) 1 (filter ( <0) xs)) ^2

test2c =  r [13] == 1 &&
          r [] == 1 &&
          r [-3,3,1,-3,2,-1] == 81 &&
          r [2,6,-3,0,3,-7,2] == 441 &&
          r [4,-2,-1,-3] == 36 


test2 = test2a && test2b && test2c

-- Question 3

data Expr = X
          | Const Int
          | Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | IfZero Expr Expr Expr
          deriving (Eq, Ord)

-- turns an Expr into a string approximating mathematical notation

showExpr :: Expr -> String
showExpr X          =  "X"
showExpr (Const n)  =  show n
showExpr (p :+: q)  =  "(" ++ showExpr p ++ "+" ++ showExpr q ++ ")"
showExpr (p :-: q)  =  "(" ++ showExpr p ++ "-" ++ showExpr q ++ ")"
showExpr (p :*: q)  =  "(" ++ showExpr p ++ "*" ++ showExpr q ++ ")"
showExpr (p :/: q)  =  "(" ++ showExpr p ++ "/" ++ showExpr q ++ ")"
showExpr (IfZero p q r)  = "(if " ++ showExpr p ++ "=0 then "
                                  ++ showExpr q ++ " else "
                                  ++ showExpr r ++ ")"

-- For QuickCheck

instance Show Expr where
    show  =  showExpr

instance Arbitrary Expr where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [X]]
                 | otherwise  =  oneof [ liftM Const arbitrary
                                       , liftM2 (:+:) subform2 subform2
                                       , liftM2 (:-:) subform2 subform2
                                       , liftM2 (:*:) subform2 subform2
                                       , liftM2 (:/:) subform2 subform2
                                       , liftM3 (IfZero) subform3 subform3 subform3
                                       ]
                 where
                   subform2  =  expr (n `div` 2)
                   subform3  =  expr (n `div` 3)

-- 3a

eval :: Expr -> Int -> Int
eval (X) x                       = x
eval (Const x) _                 = x 
eval (exp1 :+: exp2) x           = (eval exp1 x) + (eval exp2 x)
eval (exp1 :-: exp2) x           = (eval exp1 x) - (eval exp2 x)
eval (exp1 :*: exp2) x           = (eval exp1 x ) * (eval exp2 x)
eval (exp1 :/: exp2) x           = (eval exp1 x) `div` (eval exp2 x)
eval (IfZero exp1 exp2 exp3) x   = if (eval exp1 x) == 0 then (eval exp2 x) else (eval exp3 x)


test3a = eval (X :+: (X :*: Const 2)) 3                      == 9 &&
         eval (X :/: Const 3) 7                              == 2 &&
         eval (IfZero (X :-: Const 3) (X:/:X) (Const 7)) 3   == 1 &&
         eval (IfZero (X :-: Const 3) (X:/:X) (Const 7)) 4   == 7 &&
         eval (Const 15 :-: (Const 7 :/: (X :-: Const 1))) 0 == 22
-- 3 b

protect :: Expr -> Expr
protect X = X 
protect (Const x) = (Const x)
protect ( x:+: y) = (protect x) :+: (protect y )
protect ( x :-: y ) = (protect x) :-: ( protect y)
protect ( x :*: y ) = ( protect x) :*: ( protect y )
protect ( x :/: y ) = IfZero ( protect y) ( Const maxBound)((protect x) :/: ( protect y ))
protect (IfZero x y z ) = IfZero ( protect x) ( protect y )( protect z)

test3b = protect (X :+: (X :*: Const 2))== (X :+: (X :*: Const 2)) &&
         protect (X :/: Const 3) == IfZero (Const 3) (Const maxBound) (X :/: Const 3) &&
         protect (IfZero (X :-: Const 3) (X:/:X) (Const 7)) ==  IfZero (X :-: Const 3)(IfZero X (Const maxBound) (X :/: X))(Const 7) &&
         protect (Const 15 :-: (Const 7 :/: (X :-: Const 1))) == (Const 15 :-: (IfZero (X :-: Const 1)(Const maxBound)(Const 7 :/: (X :-: Const 1)))) &&
         protect (X :/: (X :-: X)) == IfZero (X :-: X) (Const maxBound) (X :/: (X :-: X))

test3c = eval (protect (X :+: (X :*: Const 2))) 3                      == 9 &&
         eval (protect (X :/: Const 3)) 7                              == 2 &&
         eval (protect (IfZero (X :-: Const 3) (X:/:X) (Const 7))) 3   == 1 &&
         eval (protect (IfZero (X :-: Const 3) (X:/:X) (Const 7))) 4   == 7 &&
         eval (protect (Const 15 :-: (Const 7 :/: (X :-: Const 1)))) 0 == 22 &&
         eval (protect (Const 15 :-: (Const 7 :/: (X :-: Const 1)))) 1 == (15-maxBound) &&
         eval (protect (X :/: (X :-: X))) 2                 == maxBound
