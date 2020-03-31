-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||),(&&)) 

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

infixr 2 ||

-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True = True
False || True = True
True || False = True
False || False = False

-------------------------------------------------------------------
------------------------------- Q1: ------------------------------- 
-------------------------------------------------------------------
infixr 3 &&

(&&) :: Bool -> Bool -> Bool
True && True = True
False && True = True
True && False = True
False && False = False

-- An alternative re-implementation of &&
--(&&) :: Bool -> Bool -> Bool
--False || False   = False
--_ && _           = True

-- Another alternative re-implementation of &&
--(&&) :: Bool -> Bool -> Bool
--True && _     =  True
--False && a    = a

-------------------------------------------------------------------
------------------------------- Q2: ------------------------------- 
-------------------------------------------------------------------

--Standard Implementation
-- ex0r :: Bool -> Bool -> Bool
-- ex0r x y = if x || y == True then True else False

--Pattern Matching Implementation
ex0r :: Bool -> Bool -> Bool
True  `ex0r` False = True
False `ex0r` True  = True
True  `ex0r` True  = False
False `ex0r` False = False


-------------------------------------------------------------------
------------------------------- Q3: ------------------------------- 
-------------------------------------------------------------------

ifThenElse :: Bool -> Int -> Int -> Int
True `ifThenElse` = 





fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m