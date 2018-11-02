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
longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList [xs] = xs
longestCommonSubList xss = foldr1 mutualsElems xss
    where 
        mutualsElems :: Eq a => [a] -> [a] -> [a]
        mutualsElems [] _ = []
        mutualsElems (x : xs) ys 
            | x `elem` ys   = x : mutualsElems xs (removeElem x ys)
            | otherwise     = mutualsElems xs ys
            where
                removeElem :: Eq a => a -> [a] -> [a]
                removeElem z zs = accepted ++ (tail rejected)
                    where
                        (accepted,rejected) = span (/=z) zs

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
canProgress :: [ModuleResult] -> Bool
canProgress ms 
    | sum credits >= 60 && all (>= 40) marks                                                                                                            = True  --Normal Pass
    | all (>= 25) marks && round (fromIntegral (sum marks) / fromIntegral (length marks)) >= 40 && sum [ (credit y) | y <- ms, (mark y) < 40] <= 15     = True  --Pass by compensation
    | otherwise                                                                                                                                         = False 
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

-- Weighting of Parts
    -- Part I work shall be excluded from the final degree classification. A weighting of 0:1:2 shall be
    -- used to obtain the Final Average Mark for the three Parts of an Honours degree programme, and
    -- a weighting of 0:1:2:2 for the four Parts of an integrated Masters programme. This is in addition
    -- to weighting by credit points (for example, where Parts III and IV do not contain the same
    -- number of credit points).

-- Classification Algorithm
    -- The class awarded shall be that within which the Final Average Mark rounded to the nearest
    -- integer falls. The next higher class will be awarded if the unrounded Final Average Mark is within
    -- 2 marks of the higher class and at least 50% of the credit points, weighted by Part, are derived
    -- from Module Marks in the higher class or above.
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)
classify :: [[ModuleResult]] -> DegreeClass
classify ms@(y1:y2:y3:[]) 
    | hasPassed == False                                                                        = error "This student has failed one of their years"
    | finalAverageMark >= 70                                                                    = First
    | finalAverageMark >= 68 && ((1/3)*(avgCredGTEQ y2 70) + (2/3)*(avgCredGTEQ y3 70)) >= 50   = First
    | finalAverageMark >= 60                                                                    = UpperSecond
    | finalAverageMark >= 58 && ((1/3)*(avgCredGTEQ y2 60) + (2/3)*(avgCredGTEQ y3 60)) >= 50   = UpperSecond
    | finalAverageMark >= 50                                                                    = LowerSecond
    | finalAverageMark >= 48 && ((1/3)*(avgCredGTEQ y2 50) + (2/3)*(avgCredGTEQ y3 50)) >= 50   = LowerSecond
    | finalAverageMark >= 40                                                                    = Third
    | finalAverageMark >= 38 && ((1/3)*(avgCredGTEQ y2 40) + (2/3)*(avgCredGTEQ y3 40)) >= 50   = Third
    | otherwise                                                                                 = error "This student has failed"
    where
        hasPassed = canProgress y1 && canProgress y2 && canProgress y3
        finalAverageMark = round ((1/3)*y2Avg + (2/3)*y3Avg)

        y2Avg = fromIntegral (sum $ getMarks y2) / fromIntegral (length y2)
        y3Avg = fromIntegral (sum $ getMarks y3) / fromIntegral (length y3)

        getCreditsWithMarkGTEQ :: [ModuleResult] -> Int -> [Float]
        getCreditsWithMarkGTEQ ms n = [ (credit m) | m <- ms, (mark m) >= n]

        avgCredGTEQ :: [ModuleResult] -> Int -> Float
        avgCredGTEQ ms n = (sum $ getCreditsWithMarkGTEQ ms n) / fromIntegral (length ms)

        getMarks :: [ModuleResult] -> [Int]
        getMarks [] = []
        getMarks (x:xs) = (mark x):getMarks xs

        getCredits :: [ModuleResult] -> [Float]
        getCredits [] = []
        getCredits (x:xs) = (credit x):getCredits xs

classify ms@(y1:y2:y3:y4:[])
    | hasPassed == False                                                                                                    = error "This student has failed one of their years"
    | finalAverageMark >= 70                                                                                                = First
    | finalAverageMark >= 68 && ((1/5)*(avgCredGTEQ y2 70) + (2/5)*(avgCredGTEQ y3 70) + (2/5)*(avgCredGTEQ y4 70)) >= 50   = First
    | finalAverageMark >= 60                                                                                                = UpperSecond
    | finalAverageMark >= 58 && ((1/5)*(avgCredGTEQ y2 60) + (2/5)*(avgCredGTEQ y3 60) + (2/5)*(avgCredGTEQ y4 60)) >= 50   = UpperSecond
    | finalAverageMark >= 50                                                                                                = LowerSecond
    | finalAverageMark >= 48 && ((1/5)*(avgCredGTEQ y2 50) + (2/5)*(avgCredGTEQ y3 50) + (2/5)*(avgCredGTEQ y4 50)) >= 50   = LowerSecond
    | finalAverageMark >= 40                                                                                                = Third
    | finalAverageMark >= 38 && ((1/5)*(avgCredGTEQ y2 40) + (2/5)*(avgCredGTEQ y3 40) + (2/5)*(avgCredGTEQ y4 40)) >= 50   = Third
    | otherwise                                                                                                             = error "This student has failed their degree"
    where
        hasPassed = canProgress y1 && canProgress y2 && canProgress y3 && canProgress y4
        finalAverageMark = round (0.2*y2Avg + 0.4*y3Avg + 0.4*y4Avg)

        y2Avg = fromIntegral (sum $ getMarks y2) / fromIntegral (length y2)
        y3Avg = fromIntegral (sum $ getMarks y3) / fromIntegral (length y3)
        y4Avg = fromIntegral (sum $ getMarks y4) / fromIntegral (length y4)

        getCreditsWithMarkGTEQ :: [ModuleResult] -> Int -> [Float]
        getCreditsWithMarkGTEQ ms n = [ (credit m) | m <- ms, mark m >= n]

        avgCredGTEQ :: [ModuleResult] -> Int -> Float
        avgCredGTEQ ms n = (sum $ getCreditsWithMarkGTEQ ms n) / fromIntegral (length ms)

        getMarks :: [ModuleResult] -> [Int]
        getMarks [] = []
        getMarks (x:xs) = (mark x):getMarks xs

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
findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers [x] = []
findBusyBeavers ns@(x:x':xs)  
    | x == 0            = (possibilities [Pop] $ findBusyBeavers (x':xs)) ++ (possibilities [Add] $ findBusyBeavers (x':xs))
    | x+x' < x*x'       = possibilities [Multiply] $ findBusyBeavers ((x*x'):xs)
    | x+x' > x*x'       = possibilities [Add] $ findBusyBeavers ((x+x'):xs)
    | x+x' == x*x'      = (possibilities [Add] $ findBusyBeavers ((x*x'):xs)) ++ (possibilities [Multiply] $ findBusyBeavers ((x*x'):xs))
    where
        possibilities :: [Instruction] -> [[Instruction]] -> [[Instruction]]
        possibilities as [] = [as]
        possibilities as bss = [ as ++ bs | bs <- bss]

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

        removeEmpties :: [Rectangle] -> [Rectangle]
        removeEmpties [] = []
        removeEmpties rects@(x:xs)
            | isEmpty x             = removeEmpties xs
            | otherwise             = x:removeEmpties xs
            where
                isEmpty :: Rectangle -> Bool
                isEmpty (Rectangle a@(xa,ya) b@(xb,yb))
                    | xa >= xb      = True
                    | ya >= yb      = True
                    | otherwise     = False
        
        contains :: Rectangle -> Rectangle -> Bool
        contains (Rectangle a@(xa,ya) b@(xb,yb)) (Rectangle c@(xc,yc) d@(xd,yd))
            | xa <= xc && xb >= xd && ya <= yc && yb >= yd      = True
            | xc <= xa && xd >= xb && yc <= ya && yd >= yb      = True
            | otherwise                                         = False

        simplify :: Rectangle -> Rectangle -> Rectangle
        simplify (Rectangle a@(xa,ya) b@(xb,yb)) (Rectangle c@(xc,yc) d@(xd,yd))
            | xa <= xc && xb >= xd && ya <= yc && yb >= yd              = Rectangle a b
            | xc <= xa && xd >= xb && yc <= ya && yd >= yb              = Rectangle c d
            | otherwise                                                 = error "Simplify call attempted to simplify two rectangles which cannot be simplified"


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
isShellTreeSum n 
    | squareShellPair (t,v) == n         = True
    | otherwise                         = False
    where
        (t,v) = reverseSquareShell n
        -- value of Tree and Value of sum of node values

        reverseSquareShell :: Int -> (Int,Int)
        reverseSquareShell z
            | z - m^2 < m       = (z-m^2,m)
            | otherwise         = (m,m^2 + 2*m - z)
            where 
                m = floor (sqrt (fromIntegral z))

        squareShellPair :: (Int,Int) -> Int
        squareShellPair (x,y) = (max x y)^2 + max x y + x - y