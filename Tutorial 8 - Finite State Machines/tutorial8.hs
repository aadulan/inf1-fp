-- Informatics 1 - Functional Programming 
-- Tutorial 8
--
-- Week 10 - due: 24/25 Nov.

import Data.List
import Test.QuickCheck
import Data.Char


-- Type declarations

type FSM q = ([q], Alphabet, q, [q], [Transition q])
type Alphabet = [Char]
type Transition q = (q, Char, q)



-- Example machines

m1 :: FSM Int
m1 = ([0,1,2,3,4],
      ['a','b'],
      0,
      [4],
      [(0,'a',1), (0,'b',1), (0,'a',2), (0,'b',2),
       (1,'b',4), (2,'a',3), (2,'b',3), (3,'b',4),
       (4,'a',4), (4,'b',4)])

m2 :: FSM Char
m2 = (['A','B','C','D'],
      ['0','1'],
      'B',
      ['A','B','C'],
      [('A', '0', 'D'), ('A', '1', 'B'),
       ('B', '0', 'A'), ('B', '1', 'C'),
       ('C', '0', 'B'), ('C', '1', 'D'),
       ('D', '0', 'D'), ('D', '1', 'D')])

dm1 :: FSM [Int] 
dm1 =  ([[],[0],[1,2],[3],[3,4],[4]],
        ['a','b'],
        [0],
        [[3,4],[4]],
        [([],   'a',[]),
         ([],   'b',[]),
         ([0],  'a',[1,2]),
         ([0],  'b',[1,2]),
         ([1,2],'a',[3]),
         ([1,2],'b',[3,4]),
         ([3],  'a',[]),
         ([3],  'b',[4]),
         ([3,4],'a',[4]),
         ([3,4],'b',[4]),
         ([4],  'a',[4]),
         ([4],  'b',[4])])



-- 1.
states :: FSM q -> [q]
alph   :: FSM q -> Alphabet
start  :: FSM q -> q
final  :: FSM q -> [q]
trans  :: FSM q -> [Transition q]


states (u,a,s,f,t) = u 
alph   (u,a,s,f,t) = a
start  (u,a,s,f,t) = s
final  (u,a,s,f,t) = f
trans  (u,a,s,f,t) = t


-- 2.
delta :: (Eq q) => FSM q -> q -> Char -> [q]
delta fsm no alp = [ c | (a,b,c) <- trans fsm , no == a && alp == b ]

--delta fsm no alp = [ if (no, alp, _) == (a,b,c) then c else [] |(a,b,c) <- trans fsm]
-- why can't I usea wildcard here?


-- 3.
accepts :: (Eq q) => FSM q -> String -> Bool
accepts m xs = acceptsFrom m ( start m) xs

acceptsFrom :: (Eq q) => FSM q -> q -> String -> Bool
acceptsFrom m q [] = q `elem` final m 
acceptsFrom m q (x:xs) = or [ acceptsFrom m y xs | y <- delta m q x] 

-- gets each of the states from all the states from delta and then it calls it recursilvely and if it returns with an empty list then it sees if the final state is an element of the accepting state.



-- 4.
canonical :: (Ord q) => [q] -> [q]
canonical xs = nub (sort xs)


-- 5.
ddelta :: (Ord q) => FSM q -> [q] -> Char -> [q]
ddelta  fsm num alp =  canonical (concat [(delta fsm x alp) | x <- num])
 
-- use head as then if you don't you get a lists of lists.

-- 6.
next :: (Ord q) => FSM q -> [[q]] -> [[q]]
next fsm xs =  canonical (xs ++ [ddelta fsm x alp | x <- xs, alp <- alph fsm])


-- 7.
reachable :: (Ord q) => FSM q -> [[q]] -> [[q]]
reachable fsm xs | next fsm xs == xs = next fsm xs
                 | otherwise = reachable fsm ( next fsm xs)


-- 8.
dfinal :: (Ord q) => FSM q -> [[q]] -> [[q]]
dfinal fsm xs = [ x |x <- xs , or [ end `elem` x | end <- final fsm]]

--we are derving the superstates from the list of superstates and then checking wheather the final states is an element in the superstate.


-- 9.
dtrans :: (Ord q) => FSM q -> [[q]] -> [Transition [q]]
dtrans fsm xs = [ (a,b, ddelta fsm a b) | a <- xs , b <- alph fsm ]


-- 10.
deterministic :: (Ord q) => FSM q -> FSM [q]
deterministic = 

-- Optional Material
--11.
charFSM :: Char -> FSM Int
charFSM = undefined

emptyFSM :: FSM Int
emptyFSM = undefined

--12
intFSM :: (Ord q) => FSM q -> FSM Int
intFSM = undefined

concatFSM :: Ord q => Ord q' => FSM q -> FSM q' -> FSM Int
concatFSM = undefined

--13
stringFSM :: String -> FSM Int
stringFSM = undefined


-- For quickCheck
safeString :: String -> String
safeString a = filter (`elem` ['a'..'z']) (map toLower a)

prop_stringFSM1 n = accepts (stringFSM n') n'
      where n' = safeString n
prop_stringFSM2 n m = (m' == n') || (not $ accepts (stringFSM n') m')
                where m' = safeString m
                      n' = safeString n

--14
completeFSM :: (Ord q) => FSM q -> FSM (Maybe q)
completeFSM = undefined

unionFSM :: (Ord q) => FSM q -> FSM q -> FSM Int
unionFSM a b = undefined
        
prop_union n m l =  accepts (unionFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l'|| accepts (stringFSM m') l') &&
                    accepts (unionFSM (stringFSM n') (stringFSM m')) n' && accepts (unionFSM (stringFSM n') (stringFSM m')) m'
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l

--15
star :: (Ord q) => FSM q -> FSM q
star = undefined

    
prop_star a n = (star $ stringFSM a') `accepts` (concat [a' | x <- [0..n]]) &&
                (star $ stringFSM a') `accepts` ""
      where a' = safeString a

--16
complement :: (Ord q) => FSM q -> FSM Int
complement = undefined

prop_complement :: String -> String -> Bool
prop_complement n m = (n' == m')
                      || accepts (complement $ stringFSM n') m'
                      && (not $ accepts (complement $ stringFSM n') n)
                      where n' = safeString n
                            m' = safeString m

-- 17.
intersectFSM :: (Ord q) => FSM q -> FSM q -> FSM (q,q)
intersectFSM a b = undefined
                
prop_intersect n m l = accepts (intersectFSM (stringFSM n') (stringFSM m')) l' == (accepts (stringFSM n') l' && accepts (stringFSM m') l')
                    where m' = safeString m
                          n' = safeString n
                          l' = safeString l



prop1 a b = star ((stringFSM a') `unionFSM` (stringFSM b')) `accepts` (a'++b'++a'++a')
 where a' = safeString a
       b' = safeString b

prop2 a b = ((stringFSM a') `intersectFSM` (intFSM ((stringFSM b') `unionFSM` (stringFSM a')))) `accepts` a'
             where a' = safeString a
                   b' = safeString b


