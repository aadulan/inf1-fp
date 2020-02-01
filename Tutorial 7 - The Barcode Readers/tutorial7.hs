-- Informatics 1 Functional Programming
-- Tutorial 7
--
-- Due: 17/18 November

import System.Random


-- Importing the keymap module

import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen xs = maximum[length j  |(i,(j,k)) <- xs]

formatLine :: Int -> (Barcode, Item) -> String
formatLine n (x,(y,z)) =  x ++ "..." ++ y ++ replicate (n -(length y)) '.' ++ "..." ++ z

showCatalogue :: Catalogue -> String
showCatalogue xs = foldr (++) "" [formatLine (longestProductLen (toList xs)) y ++ "\n" | y <- toList xs]

-- "\n" - enters the thing into a next line to make it look neat :)



--showCatalogue xs  = map (formatLine (longestProductLen (toList xs))) (toList xs)
-- why can't I use map?
     
-- Exercise 2

--get "9780201342758" testDB = Just ("Thompson - \"Haskell: The Craft of Functional Programming\"","Book")
-- get "000" testDB = Nothing

--A) get  - Maybe a ( Just a or Nothing)


maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

--maybesToList takes the output of get and returns the thing without just or an empty list

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing 
listToMaybe xs = Just (head xs)

catMaybes :: [Maybe a] -> [a]
catMaybes [] =[]
catMaybes (x:xs) = maybeToList x ++ catMaybes xs 

--catMaybes :: [Maybe a] -> [a]
--catMaybes xs = foldr (++) [][ maybeToList x | x<- xs] 

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems xs ys = catMaybes [ get x ys | x <-xs]


--Exercise 4
-- theDB <- readDB : 0.62
-- it would take an average of 0.02 seconds 
-- the get function would search through every item in the list

--Exercise 11 
-- (3.64 secs, 2102310584 bytes)
-- to use getSample average of between 0.07-0.10 seconds 
-- to use get it average of 0.00 seconds.



-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)
