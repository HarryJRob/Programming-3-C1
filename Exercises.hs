-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, 
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond, 
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     

--TODO: Remove me
import Debug.Trace (trace)


-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
splitSort :: Ord a => [a] -> [[a]] 
splitSort [] = []
splitSort [x] = [[x]]
splitSort ns@(x:x':xs) = (x:x':(map snd out)):splitSort (map snd remaining)
    where 
        state = compare x x'
        (out,remaining) = span ((==state) . uncurry compare) (zip (x':xs) xs)
        -- ((==state) . uncurry compare)
        -- uncurry compare creates a function which works on tuples
        -- '.' composes the two functions
        -- Thefore only when the comparison of the current tuple results the same as the state previously determined then it will be added to the 'out' list
        -- This is done until the condition is failed which is then added to the 'remaining' list

-- Exercise 2
-- longest common sub-list of a finite list of finite list
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList [xs] = xs
longestCommonSubList xss = foldr1 intersection xss
    where 
        intersection xs ys = [ x | x <- xs , y <- ys, x == y]

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress ms = False

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms = Third 

-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps
        | (x' - x) <= sqrt eps   = (x' + x)/2
        | d x2 > d x1           = hillClimb d x x1 eps
        | otherwise             = hillClimb d x2 x' eps
        where
            invphi      = (sqrt 5 - 1) /2
            dist        = invphi*(x' - x)
            x1          = x + dist
            x2          = x' - dist

-- d = function
-- x'= upper boundary of the search
-- x = lower boundary of the search
-- eps = tolerance
-- x1 = first comparison point phi ratio from the upper bound / total width
-- x2 = second comparison point phi ratio from the lowerbound / total width


-- Exercise 6
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = 0.0

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns ins = []

-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence n = [] 

-- Exercise 9
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers ns = []

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList rs = []

-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []

-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = ""

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
differentStream ss = []

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f 0 0

-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n = False