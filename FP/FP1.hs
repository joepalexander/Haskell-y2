timesTen :: Int -> Int
timesTen n = n * 10

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

areaOfCircle :: Float -> Float
areaOfCircle r = (pi) * (r^2)

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder r h  = h * ((pi) * (r^2))

distance :: Float -> Float -> Float -> Float -> Float
distance y1 y2 x1 x2 = sqrt(((y1-y2)^2) + ((x1-x2)^2))

threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = if x/= y then if z /= y then if z /= x then True else False else False else False

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = if (mod x y) == 0 then True else False

isEven :: Int -> Bool
isEven x = if (divisibleBy x 2) == True then True else False

averageThree :: Int -> Int -> Int -> Float
averageThree x y z = fromIntegral (x + y + z) / 3

absolute :: Int -> Int
absolute n = if n >= 0 then n else -n


