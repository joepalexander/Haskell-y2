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
