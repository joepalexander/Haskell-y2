import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)

sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x+y, x-y)

grade :: StudentMark -> Char
grade y x 
    | x >= 70 = (y,'A')
    | 60 < x && x < 69 = (y, 'B')
    | 50 < x && x < 59 = (y, 'C')
    | 40 < x && x < 49 = (y, 'D')
    | x <= 39 = (y, 'F')
        -- x >= 70 = 'A'
        -- 60 < x && x < 69 = 'B'
        -- 50 < x && x < 59 = 'C'
        -- 40 < x && x < 49 = 'D'
        -- x <= 39 = 'F'