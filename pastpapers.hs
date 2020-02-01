
import Data.List
import Data.Char


isNext :: Int -> Int -> Bool 
isNext x y | even x && y == x `div` 2 = True
           | odd x && y == ((x*3) +1 ) = True 
           | otherwise = False

collatz :: [Int] -> Bool 
collatz [] = True
collatz xs = and [ isNext x y |( x,y) <- zip (init xs)( drop 1 xs)]    --x <- (init xs) , y <- (drop 1 xs) ]

collatzRec :: [Int] -> Bool
collatzRec [] = True 
collatzRec [x] = True 
collatzRec (x:xs) | isNext x (head xs) = True && collatzRec xs 
                  | otherwise = False

f :: Char -> Bool
f x | elem x "abcdefghijklm" = True 
    | elem x "ABCDEFGHIJKLM" = True 
    | isAlpha x = False
    | otherwise = error "Hi there, the char you have given me is not actually a alphabet, thanks :) "



f1 :: Char -> Bool
f1 x | elem x "abcdefghijklm" || elem x "ABCDEFGHIJKLM" = True
    | isAlpha x = False
    | otherwise = error "Hi there, the char you have given me is not actually a alphabet, thanks :) "

g:: String -> Bool 
g xs =  (length l) > (length xs - length l) 
  where  l = [x | x<-xs , isAlpha x && f x]

h:: String -> Bool 
h ( x:xs) = length l > ( length xs - length l)
 where l | isAlpha x && f x = x : l
         | otherwise = l

c:: [Int] -> [Int]
c [] =[]
c xs = [ x| (x,y) <- zip xs ( drop 1 xs) , x==y]

d:: [Int] -> [Int] 
d [] = []
d (x:xs) | x == head xs = x : c xs
         | otherwise = c xs


