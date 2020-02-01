-- Informatics 1 Functional Programming
-- December 2015
-- SITTING 2 (14:30 - 16:30)

import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized, (==>), Property )
import Control.Monad -- defines liftM, liftM3, used below
import Data.List
import Data.Char

-- Question 1

-- 1a

p :: [Int] -> Int
p xs =  (1 + (((sum [x| x<-xs, x>0]) `div` 24) `mod` 7))

test1 = p [] == 1 &&
        p [-30,-20] == 1 &&
        p [12,-30,7,8,-20] == 2 &&
        p [90,15] == 5 &&
        p [90,-100,23,-20,54] == 7 &&
        p [90,-100,23,-20,55] == 1

-- 1b

q :: [Int] -> Int
q = undefined

-- 1c

r :: [Int] -> Int
r = undefined

-- Question 2

-- 2a

f :: String -> String
f = undefined

-- 2b

g :: String -> String
g = undefined

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
r6 = Seq (Lit 'B')
         (Seq (Lit 'A')
              (Or (Lit 'C') (Lit 'D')))       -- B(A(C|D))

r1' = Or (Seq (Lit 'A') (Lit 'A'))
         (Seq (Lit 'A') (Lit 'A'))            -- (AA)|(AA)
r2' = Or (Seq (Or (Lit 'A') Epsilon)
              (Lit 'A'))
         (Seq (Or (Lit 'A') Epsilon)
              (Lit 'B'))                     -- ((A|e)A)|((A|e)B)
r3' = Or (Seq (Or (Lit 'A')
                  (Seq Epsilon (Lit 'A')))
              (Lit 'A'))
         (Seq (Or (Lit 'A')
                  (Seq Epsilon (Lit 'A')))
              (Lit 'B'))                     -- ((A|(eA))A) | ((A|(eA))B)
r4' = r4                                      -- (A|(eA))((A|B)e)
r5' = Seq (Or (Seq (Or (Lit 'A')
                       (Seq Epsilon (Lit 'A')))
                   Epsilon)
              (Seq (Or (Lit 'A')
                       (Seq Epsilon (Lit 'A')))
                   (Lit 'B')))
          (Seq (Or (Lit 'A') (Lit 'B'))
               Epsilon)                       -- (((A|(eA))e)|((A|(eA))B))((A|B)e)
r6' = Or (Seq (Lit 'B')
              (Seq (Lit 'A') (Lit 'C')))
         (Seq (Lit 'B')
              (Seq (Lit 'A') (Lit 'D')))      -- (B(AC))|(B(AD))

-- 3a

language :: Regexp -> [String]
language = undefined

-- 3b

flatten :: Regexp -> Regexp
flatten = undefined