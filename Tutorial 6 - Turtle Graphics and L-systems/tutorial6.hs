-- Informatics 1 - Functional Programming 
-- Tutorial 6
--
-- Week 8 - Due: 10/11 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Sit :#: p) = [p]
split (p :#: Sit) = [p]
split (p :#: q) = split p ++ split q
split (p) = [p]

-- 1b. join
join1 :: [Command] -> Command
join1 [] =  Sit
join1 (x:xs) = x :#: join1 xs

join2 :: [Command] -> Command
join2 [x] =  x
join2 (x:xs) = x :#: join1 xs



-- 1c  equivalent
equivalent xs ys = split xs == split ys

-- 1d. testing join and split
prop_split_join c = join1 (split c) == c 

prop_split c = not ( Sit `elem` (split c))

--prop_split :: [Command] -> Bool
--prop_split [] = True
--prop_split (x:xs) | x == :#: && Sit = False
                  -- | otherwise = prop_split xs

--prop_split xs = split (join1 xs) == xs
--prop_split (x:xs) | x == :#: && Sit = False
                  -- | otherwise = prop_split xs

--prop_split = undefined


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 1 ys = ys
copy x ys  = ys :#: copy (x-1) ys

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon x = copy 5 ( Go x :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon x y = copy y ( Go x :#: Turn ( 180-(fromIntegral((y-2) * 180)/ fromIntegral y)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral s 1 st a = Go s :#: Turn a
spiral s n st a = (Go s :#: Turn a) :#: spiral (s + st) (n-1) st a


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise xs = join1 (filter ( /= Sit)( map help (split xs)))

--optimise xs = join2 (split( filter ( not Sit)( map help xs)))

--optimise xs = split (join2(( filter (not Sit)(map help xs))))

help :: Command -> Command
help Sit = Sit  
help (Go 0) = Sit
help (Turn 0) = Sit
help (Go 0 :#: Go a) = Go a 
help (Go a :#: Go 0) = Go a 
help (Turn 0 :#: Turn a) = Turn a
help (Turn a :#: Turn 0) = Turn a
help (Go a :#: Go b) = Go (a+b) 
help (Turn a :#: Turn b) = Turn (a+b)
help (Turn a :#: Go b) = (Turn a :#: Go b)
help (Go a :#: Turn b) = (Go a :#: Turn b)

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead y =f y
 where 
     f 0 =  Go 10 
     f y  = g (y-1) :#: Turn 60 :#: f (y-1) :#: Turn 60 :#: g (y-1)
     g 0  =  Go 10 
     g y =  f (y-1) :#: Turn (-60) :#: g (y-1) :#: Turn (-60) :#: f (y-1)

-- 6. snowflake
snowflake :: Int -> Command
snowflake y = f y :#: Turn 60 :#: Turn 60 :#: f y :#: Turn 60 :#: Turn 60 :#: f y :#: Turn 60 :#: Turn 60
 where 
 f 0 = Go 10 
 f y = f(y-1) :#: Turn (-60) :#: f(y-1) :#: Turn 60 :#: Turn 60 :#: f (y-1) :#: Turn (-60) :#: f(y-1)

-- 7. hilbert
hilbert :: Int -> Command
hilbert y =  l y
 where 
 l 0 = Go 10 
 l y = Turn 90 :#: r (y-1) :#: Go 10 :#: Turn (-90) :#: l (y-1) :#: Go 10  :#: l (y-1) :#: Turn (-90) :#: Go 10 :#: r (y-1) :#: Turn 90
 r 0 = Go 10 
 r y = Turn (-90) :#: l (y-1) :#: Go 10 :#: Turn 90 :#: r (y-1) :#:Go 10 :#: r ( y-1) :#: Turn 90 :#: Go 10 :#: l (y-1) :#: Turn (-90)


