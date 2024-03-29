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

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent xs ys = split xs == split ys

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join c = join1 (split c) == c

--prop_split :: [Command] -> Bool
--prop_split [] = True
--prop_split (x:xs) | x == :#: && Sit = False
                  -- | otherwise = prop_split xs

--prop_split xs = split (join1 xs) == xs
--prop_split (x:xs) | x == :#: && Sit = False
                  -- | otherwise = prop_split xs


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 1 ys = ys
copy x ys = ys :#: copy (x-1) ys

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon x = copy 5 (Go x :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon x y = copy y ( Go x :#: Turn (180 -(fromIntegral((y-2) * 180)/fromIntegral y)))



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
sprial side 1 step angle =  Go side :#: Turn angle
spiral side n step angle = (Go side :#: Turn angle) :#: (spiral (side + step) (n-1) step angle)


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = undefined



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

