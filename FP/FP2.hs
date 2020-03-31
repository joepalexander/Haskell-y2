absolute :: Int -> Int
absolute x
  | x<0 = -x
  | otherwise = x
  
sign :: Int -> Int
sign x
  | x<0 = -1
  | x>0 = 1
  | otherwise = x
  
howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
  | x == y && x == z && y == z = 3
  | x == y = 2 | x == z = 2 | y == z = 2
  | otherwise = 0
 

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = (squareR2) * (sum)
                            where
                            sum = (x + y + z)
                            squareR2 = sqrt(fromIntegral 2)

howManyAboveAverage :: Int -> Int -> Int -> Int
averageThree x y z = (fromIntegral x+ fromIntegral y+ fromIntegral z)/3
howManyAboveAverage x y z = length [ i | i <- [x, y, z], fromIntegral i > avg]
                            where avg = averageThree x y z				
							
validDate :: Int -> Int -> Bool
validDate days month 
   | (month == 2) && days > 28 = False
   | (month == 9 || month == 4 || month == 6 || month == 11) && days > 30 = False
   | days > 31 = False
   | month > 12 = False
   | otherwise = True

daysInMonth :: Int -> Int -> Int
daysInMonth month year
  | (year `mod` 4 == 0) && (month == 2) = 29
  | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 = 31
  | month == 4 || month == 6 || month == 9 || month == 11 = 30
  | otherwise = 28
