import Data.Char
import Test.QuickCheck

-- 1a

f :: [String] -> String
f xs = concat[ x| x<- xs , help x]
 where help (y:ys) = isUpper y

-- 1b

g :: [String] -> String
g =  undefined

-- 1c

h :: [String] -> String
h =  undefined

-- 2a

p :: [a] -> [a]
p =  undefined

-- 2b

q :: [a] -> [a]
q =  undefined

-- 3a

data Term  =  Con Int
            | X
            | Term :+: Term
            | Term :*: Term

eva :: Term -> Int -> Int
eva =  undefined

-- 3b

sho :: Term -> String
sho =  undefined
