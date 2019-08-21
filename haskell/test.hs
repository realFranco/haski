-- 18*08*19 
-- filename: test.hs
-- dev: f97gp1@gmail.conm
-- use ghci for sart the interactive bash
--
-- :load namefile.hs for load the file and interact with it
-- :m for back from the code to the prelude section
-- 
-- for execute use:
--      :load test.hs
--      sayHello "Name" # Hello, Name!
-- using :r -> will reload the modulo
-- using :t functionName -> will show the definition of the funcion

-- This code will cover some code sections explained by
-- Derecck Banas ~ Haskell Tutorial https://www.youtube.com/watch?v=02_H3LjqMr8


-- module declaration are capitalized
module Test where
import System.IO
import Data.List

-- camel case
-- sayHello :: String -> IO ()
-- sayHello x =
--     putStrLn ("Hello, " ++ x ++ "!")

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in
    sideArea + 2 * topArea


addMe :: Int -> Int -> Int
addMe  x y = x + y


addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) = (x + x2, y + y2)


whatAge :: Int -> String
whatAge 15 = "Can't drive"
whatAge 18 = "Can drive"
whatAge x = "Other Age: " ++ show x


-- negaribe numbers need to be inside of the parenthesis
-- values (-1)
-- 
values :: Integer -> Integer
values n
  | n <  0   = (-1)
  | n == 0  = 0
  | n > 0   = 1


factorial :: Integer -> Integer
factorial n
    | n == 0 = 1
    | n > 0 = n * factorial( n-1 )


prodFactorial n = product[1..n]


-- use  a back quote (`)
isOdd :: Integer -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True


isEven n = n `mod` 2 == 0


raceAverage :: Double -> Double -> String
raceAverage race victory
	| avg == 1 					= "Great driver"
	| (avg >= 0.3) && (avg < 1) = "Almost a Great Drive"
	| (avg > 0) && (avg < 0.3) 	= "Bad Driver"
	| otherwise 				= "You are not a driver: "
	where avg = race / victory
	

data Punto = DualInt Integer Integer

  
-- a será de caulquier tipo
data Slot1 a = Slot1 a


getListItems :: [Integer] -> String
getListItems [] = "Your list is empty "
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list constains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The 1st item is " ++ show x ++ 
						" and the rest are " ++ show xs


getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]


-- times4 :: Integer -> Integer
-- times4 x = x * 4


listTimes4 = map times4 [1,2,3,4,5]


--pass the first item, after pass the rest of the imtes using xs
multBy4 :: [Integer] -> [Integer]
multBy4 [] = []
multBy4 (x:xs) = times4 x : multBy4 xs 	


areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False


times4 :: Integer -> Integer
times4 x = x * 4


-- Recieving an function, return an Integer
doMult :: (Integer -> Integer) -> Integer
doMult func = func 3


num3Times4 = doMult times4 -- return 12


-- return a function
getAddFunc :: Integer -> (Integer -> Integer)
getAddFunc x y = x + y

adds3 = getAddFunc 3

fourPlus3 = adds3 4

treePlusList = map adds3 [1,2,3]


data BaseballPlayer = Pitcher
					| Catcher
					| Infielder
					| OutField
				deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds OutField = True

barryInOF = print( barryBonds OutField)

-- barryInCT = print( barryBonds Catcher)


data Customer = Customer String String Double
	deriving Show
	
francois :: Customer
francois = Customer "Fanco Gil" "Haskell" 11.22

getBalance :: Customer -> Double
getBalance (Customer a a2 b) = b


data RPS = Rock | Paper | Scissors

shoot ::RPS -> RPS -> String
shoot Paper Rock = "Paper beats Rock"
shoot Scissors Rock = "Scissors lose Rock"
shoot _ _ = "Error on the values"
			

data Shape s = Circle Float Float Float | Rectangle Float Float Float Float
	deriving Show
	
area :: Shape s -> Float
area (Circle _ _ r) = pi * r ^ 2
-- area (Rectangle x y x2 y2) = abs(x2 - x) * abs(y2 -y)
area (Rectangle x y x2 y2) = (abs $ x2 - x) * (abs $ y2 - y)
 
c1 =  Circle 1 2 3
r1 = Rectangle 1 2 3 4

sumValue = putStrLn( show(1 + 2))
sumValue2 = putStrLn . show $ 1 + 2

areaofCircle = area (Circle 10 20 20)
areaofRectangle = area $ Rectangle 10 10 100 100


class MyEq a where
	areEqual :: a -> a -> Bool

data ShirtSize = S | M | L

instance MyEq ShirtSize where
	areEqual S S = True
	areEqual M M = True
	areEqual L L = True
	areEqual _ _ = False

newSize = areEqual M M


sayHello = do
	putStrLn "What's your name"
	name <- getLine
	putStrLn $ "Hello " ++ name
 

fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

-- !! returns the element that is in the specified
-- position in the list
fib300 = fib !! 300

concat = 'c' : "hris"


getElements :: [Int] -> Int -> Int
getElements ll x
				| x-1 < length ll = ll !! x

getSix = getElements [11,12,13,14,15,16,17,18] 6
getNine = getElements [11,12,13,14,15,16,17,18] 9


-- data Digrafo v = G [v] deriving Show
-- g1 = G [1,2,3]
-- show g1

-- data Digrafo v = G [v] (v -> [v])

-- grafo1 = (G [1..4] suc) where
-- 	suc 1 = [2,3]
-- 	suc 2 = [4]
-- 	suc 3 = [4]
-- 	suc 4 = []

-- vertices :: Digrafo v -> [v]
-- vertices g = 


-- data Digrafo v = G { vertex :: [v],  nodes:: (v, [v])}

-- g1 = G ( [1,2,3], [ (1, [2]), (2, [3]) ] ) 

-- g1 = G { vertex=[1,2,3,4], nodes=(1, [2,3])
  
data Some v = SD ([v], [v])

let s1 = SD(  [1,2,3], [1,2,3])

