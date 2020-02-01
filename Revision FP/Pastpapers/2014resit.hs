-- Informatics 1 Functional Programming
-- August 2015

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>)  )
import Control.Monad -- defines liftM, liftM3, used below
import Data.Char

-- Question 1

-- 1a

f :: [a] -> [a] -> [a]
f xs ys = concat[[x,y]| (x,y) <-zip(xs)(ys)]

test1a = f "itrev" "nelae"       == "interleave" &&
         f "arp" "butmore"       == "abrupt" &&
         f [] [1,2,3]            == [] &&
         f [1,1,1] [33,11,22,44] == [1,33,1,11,1,22]

--Tuples didnt work, x++y didn't work, x:y didnt work

-- 1b

g :: [a] -> [a] -> [a]
g [] ys = []
g xs [] = []
g (x:xs)(y:ys) = x:y: g xs ys

test1b = g "itrev" "nelae"       == "interleave" &&
         g "arp" "butmore"       == "abrupt" &&
         g [] [1,2,3]            == [] &&
         g [1,1,1] [33,11,22,44] == [1,33,1,11,1,22]

-- Question 2

-- 2a

p :: [Int] -> Bool
p xs  =and [(x^2)`mod` 2 == 1 |x <- xs, even x && x>0 ]

test2a = p [13]               ==  True &&
         p []                 ==  True &&
         p [-3,3,1,-3,2,-1]   ==  False &&
         p [3,7,-3,0,3,-7,5]  ==  True &&
         p [4,-2,5,-3]        ==  False

-- strictly postive numbers means even numbers greater than zero

-- 2b

q :: [Int] -> Bool
q [] = True
q (x:xs) | x>0 && even x = ((x^2) `mod` 2 ==1) && q xs
         | otherwise = q xs

test2b = q [13]               ==  True &&
         q []                 ==  True &&
         q [-3,3,1,-3,2,-1]   ==  False &&
         q [3,7,-3,0,3,-7,5]  ==  True &&
         q [4,-2,5,-3]        ==  False

-- 2c


r :: [Int] -> Bool
r  xs = foldr (&&) True (map oddSquare (filter positive xs))   

test2c = r [13]               ==  True &&
         r []                 ==  True &&
         r [-3,3,1,-3,2,-1]   ==  False &&
         r [3,7,-3,0,3,-7,5]  ==  True &&
         r [4,-2,5,-3]        ==  False

positive :: Int-> Bool
positive x = x>0 && even x

oddSquare :: Int -> Bool
oddSquare x = odd (x^2)


-- Question 3

data Tree = Empty
          | Leaf Int
          | Node Tree Int Tree
        deriving (Eq, Ord, Show)

data Direction = L | R
        deriving (Eq, Ord, Show)

type Path = [Direction]

-- For QuickCheck

instance Arbitrary Tree where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [Empty]]
                 | otherwise  =  oneof [ liftM Leaf arbitrary
                                       , liftM3 Node subform arbitrary subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

instance Arbitrary Direction where
    arbitrary     = oneof [return L, return R]

-- For testing

t = Node (Node (Node (Leaf 1)
                     2
                     Empty)
               3
               (Leaf 4))
         5
         (Node Empty
               6
               (Node (Leaf 7)
                     8
                     (Leaf 9)))

t' = Node (Node (Node (Leaf 9)
                      8
                      (Leaf 7))
                6
                Empty)
          5
          (Node (Leaf 4)
                3
                (Node Empty
                      2
                      (Leaf 1)))

present :: Path -> Tree -> Bool
present [] (Leaf n) = True
present [] (Node _ n _) = True
present (L:p) (Node t _ _) = present p t
present (R:p) (Node _ _ t) = present p t
present _ _ = False

-- 3a

label :: Path -> Tree -> Int
label [] (Node _ t _)    = t
label p (Leaf n)         = n
label (L:p)(Node t _ _ ) = label p t
label (R:p)(Node _ _ t)  = label p t 

test3a = label [] t == 5 &&
         label [L] t == 3 &&
         label [R] t == 6 &&
         label [R,R] t == 8 &&
         label [R,R,L] t == 7

--Needed to look at the function present in order to work out the question.

-- 3b

type FTree = Path -> Int

toFTree :: Tree -> FTree
toFTree t p = label p t 

--toFTree (Node _ t _ ) [] = t
--toFTree (Leaf n) p = n
--toFTree (Node t _ _) (L:p) = toFTree t p
--toFTree (Node _ _ t) (R:p) = toFTree t p 


--opposite of label in output types 

-- 3c

mirrorTree :: Tree -> Tree
mirrorTree (Leaf n) = Leaf n 
mirrorTree Empty = Empty
mirrorTree (Node x y z ) = Node (mirrorTree z) y (mirrorTree x)

test3c = mirrorTree t == t'

--3d

mirrorFTree :: FTree -> FTree
mirrorFTree f = f . (map opposite) 
 where opposite R = L
       opposite L = R 
