-- Informatics 1 Functional Programming
-- August 2014

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )
import Control.Monad -- defines liftM, liftM2, used below
import Data.Char

-- Question 1

-- 1a

f :: String -> String
f xs = concat[ replicate x y |(x,y) <- zip[1..(length xs)] xs ]

test1a = f "abcde"   ==  "abbcccddddeeeee" &&
         f "ZYw"     ==  "ZYYwww" &&
         f ""        ==  "" &&
         f "Inf1FP"  ==  "Innfff1111FFFFFPPPPPP"

-- 1b

g :: String -> String
g [] = []
g xs = a 1 xs
  where a i [] = []
        a i (x:xs) = replicate i x ++ a (i+1) xs

  

test1b = g "abcde"   ==  "abbcccddddeeeee" &&
         g "ZYw"     ==  "ZYYwww" &&
         g ""        ==  "" &&
         g "Inf1FP"  ==  "Innfff1111FFFFFPPPPPP"

-- way to check answers 
prop1 :: String -> Bool 
prop1 xs = f xs == g xs

check1 = quickCheck prop1


-- Question 2

-- 2a

p :: [String] -> Int
p xs = sum[length x | x<- xs ,elem '.' x]

test2a = p ["Dr.","Who","crossed","the","ave."] == 7 &&
         p ["the","sgt.","opened","the","encl.","on","Fri.","pm"] == 13 &&
         p [] == 0 && 
         p ["no","abbreviations","4U"] == 0

-- 2b

q :: [String] -> Int
q [] = 0
q (x:xs) | elem '.' x = length x + q xs
         | otherwise = q xs

test2b =  p ["Dr.","Who","crossed","the","ave."] == 7 &&
          p ["the","sgt.","opened","the","encl.","on","Fri.","pm"] == 13 &&
          p [] == 0 &&
          p ["no","abbreviations","4U"] == 0
-- 2c

r :: [String] -> Int
r xs = foldr (+) 0 ( map (\x -> length x) (filter (elem '.') xs))

test2c = r ["Dr.","Who","crossed","the","ave."] == 7 &&
         r ["the","sgt.","opened","the","encl.","on","Fri.","pm"] == 13 &&
         r [] == 0 &&
         r ["no","abbreviations","4U"] == 0

-- Question 3

data Tree = Empty
          | Leaf Int
          | Node Tree Tree
        deriving (Eq, Ord, Show)

-- For QuickCheck

instance Arbitrary Tree where
    arbitrary  =  sized expr
        where
          expr n | n <= 0     =  oneof [elements [Empty]]
                 | otherwise  =  oneof [ liftM Leaf arbitrary
                                       , liftM2 Node subform subform
                                       ]
                 where
                   subform  =  expr (n `div` 2)

-- For testing

t1 = Empty

t2 = Node (Leaf 1)
          Empty

t3 = Node (Node (Node (Leaf 3)
                      Empty)
                (Leaf 1))
          (Node Empty
                (Node (Leaf 3)
                      (Leaf 5)))

t4 = Node (Node (Node Empty
                      Empty)
                (Leaf 1))
          (Node Empty
                (Node Empty
                      Empty))

-- 3a

leafdepth :: Tree -> Int
leafdepth (Empty) = 0
leafdepth (Leaf x) = 1
leafdepth (Node  x y ) = 1 +  max (leafdepth x) (leafdepth y)


test3a = leafdepth t1 == 0 &&
         leafdepth t2 == 2 &&
         leafdepth t3 == 4 &&
         leafdepth t4 == 3

-- 3 b

deepest1 :: Tree -> [Int]
deepest1 = undefined

-- 3c

deepest2 :: Tree -> [Int]
deepest2 = undefined
