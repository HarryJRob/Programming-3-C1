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
-- Currently does not do as intended
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
canProgress ms 
    | sum credits >= 60 && all (>= 40) marks                                                                        = True  --Normal Pass
    | all (>= 25) marks && sum marks `div` length marks >= 40 && sum [ (credit y) | y <- ms, (mark y) < 40] <= 15   = True  --Pass by compensation
    | otherwise                                                                                                     = False 
    where 
        marks = getMarks ms
        credits = getCredits ms

        getCredits :: [ModuleResult] -> [Float]
        getCredits [] = []
        getCredits (x:xs) = (credit x):getCredits xs

        getMarks :: [ModuleResult] -> [Int]
        getMarks [] = []
        getMarks (x:xs) = (mark x):getMarks xs

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classift ms = error "Invalid number of years of results passed"
classify ms@(y1:y2:y3:[]) 
    | hasPassed == False    = error "This student has not passed all of their years"
    | = Third
    | = LowerSecond
    | = UpperSecond
    | = First
    where
        hasPassed = canProgress y1 && canProgress y2 && canProgress y3


classify ms@(y1:y2:y3:y4:[]) = Third
    | hasPassed == False    = error "This student has not passed all of their years"
    | = Third
    | = LowerSecond
    | = UpperSecond
    | = First
    where
        hasPassed = canProgress y1 && canProgress y2 && canProgress y3 && canProgress y4

-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps
    | (x' - x) <= sqrt eps  = (x' + x)/2
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
nearestRoot xs x x' eps = hillClimb f x x' eps
    where
        ys = zip xs [0..]
        f x = (-1)*(foldr1 (+) (map (\(a,b) -> a*x^b ) ys))^2

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence ns [] = ns
executeInstructionSequence [] ins = error "Invalid Instruction Sequence - The stack is empty"
executeInstructionSequence ns@(x:[]) ins@(y:ys)
    | y == Duplicate        = executeInstructionSequence (x:x:[]) ys
    | y == Pop              = executeInstructionSequence [] ys
    | otherwise             = error "Invalid Instruction Sequence - Attempted to apply a operation which takes two elements to single element stack"

executeInstructionSequence ns@(x:x':xs) ins@(y:ys)
    | y == Add              = executeInstructionSequence ((x+x'):xs) ys
    | y == Multiply         = (x*x'):executeInstructionSequence ((x*x'):xs) ys
    | y == Duplicate        = executeInstructionSequence (x:x:x':xs) ys
    | y == Pop              = executeInstructionSequence (x':xs) ys
    | otherwise             = error "Invalid Instruction Sequence - Invalid instruction given"


-- Exercise 8
optimalSequence :: Int -> [Instruction]
optimalSequence n 
    | n == 0                        = []
    | (round b) == (ceiling b)      = concat (replicate (round b) [Duplicate,Multiply] )
    | otherwise                     = (replicate (n-1) Duplicate ++ replicate (n-1) Multiply)
        where 
            b = logBase 2 (fromIntegral n)

-- Exercise 9
-- This passes the tests however I do not believe this to be a correct solution
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [x] = []
findBusyBeavers ns@(x:x':xs)  
    | x == 0            = [Pop]:[Add]:findBusyBeavers (x':xs)
    | x+x' < x*x'       = [Multiply]:findBusyBeavers ((x*x'):xs)
    | x+x' > x*x'       = [Add]:findBusyBeavers ((x+x'):xs)
    | x+x' == x*x'      = [Add]:[Multiply]:findBusyBeavers ((x*x'):xs)
    | otherwise         = [Pop]:findBusyBeavers (x':xs)

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)
simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList [] = []
simplifyRectangleList [x] 
    | isEmpty x     = []
    | otherwise       = [x]
    where
        isEmpty :: Rectangle -> Bool
        isEmpty (Rectangle a@(xa,ya) b@(xb,yb))
            | xa >= xb      = True
            | ya >= yb      = True
            | otherwise     = False

simplifyRectangleList rs
    | notEmpty == []                = []
    | contains y y'                 = (simplify y y'):simplifyRectangleList ys
    | otherwise                     = y:y':simplifyRectangleList ys
    where
        notEmpty@(y:y':ys) = removeEmpties rs

        isEmpty :: Rectangle -> Bool
        isEmpty (Rectangle a@(xa,ya) b@(xb,yb))
            | xa >= xb      = True
            | ya >= yb      = True
            | otherwise     = False

        removeEmpties :: [Rectangle] -> [Rectangle]
        removeEmpties [] = []
        removeEmpties rects@(x:xs)
            | isEmpty x             = removeEmpties xs
            | otherwise             = x:removeEmpties xs

        contains :: Rectangle -> Rectangle -> Bool
        contains (Rectangle a@(xa,ya) b@(xb,yb)) (Rectangle c@(xc,yc) d@(xd,yd))
            | xa <= xc && xb >= xd && ya <= yc && yb >= yd      = True
            | xc <= xa && xd >= xb && yc <= ya && yd >= yb      = True
            | otherwise                                         = False


        simplify :: Rectangle -> Rectangle -> Rectangle
        simplify (Rectangle a@(xa,ya) b@(xb,yb)) (Rectangle c@(xc,yc) d@(xd,yd))
            | xa <= xc && xb >= xd && ya <= yc && yb >= yd              = Rectangle a b
            | xc <= xa && xd >= xb && yc <= ya && yd >= yb              = Rectangle c d
            | otherwise                                             = error "Simplify call attempted to simplify two rectangles which cannot be simplified"


-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = []

-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage s = convert (extract s)
    where 
        extract :: String -> String
        extract [] = []
        extract (x:xs) 
            | [x] == "0" || [x] == "1"      = [x] ++ (extract xs)
            | otherwise                     = extract xs

        convert :: String -> String
        convert [] = []
        convert (x:x':xs)
            | input == "00"     = "a" ++ (convert xs)
            | input == "01"     = "b" ++ (convert xs)
            | input == "10"     = "c" ++ (convert xs)
            | input == "11"     = "d" ++ (convert xs)
            where
                input = [x] ++ [x']

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method 
differentStream :: [[Int]] -> [Int]
differentStream ss = []

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f a b
    where
        (a,b) = reverseSquareShell n

        reverseSquareShell :: Int -> (Int,Int)
        reverseSquareShell z
            | z - m^2 < m       = (z-m^2,m)
            | otherwise         = (m,m^2 + 2*m - z)
            where 
                m = floor (sqrt (fromIntegral z))


-- Exercise 15
isShellTreeSum :: Int -> Bool
isShellTreeSum n = False