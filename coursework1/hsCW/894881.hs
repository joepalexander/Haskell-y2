
-- MATHFUN
-- Haskell Coursework Assignment
-- UP894881




--
-- Types (define Place type here)
import Data.Char
import Data.List
import Text.Printf
import Data.Foldable
import Data.Ord


type Place = (String, (Float, Float), [Int])
--

testData :: [Place]
testData = [("London", (51.5, -0.1), [0,0,5,8,8,0,0]),
            ("Cardiff", (51.5, -3.2), [12,8,15,0,0,0,2]),
            ("Norwich", (52.6, 1.3), [0,6,5,0,0,0,3]),
            ("Birmingham", (52.5, -1.9), [0,2,10,7,8,2,2]),
            ("Liverpool", (53.4, -3.0), [8,16,20,3,4,9,2]),
            ("Hull", (53.8, -0.3), [0,6,5,0,0,0,4]),
            ("Newcastle", (55.0, -1.6), [0,0,8,3,6,7,5]),
            ("Belfast", (54.6, -5.9), [10,18,14,0,6,5,2]),
            ("Glasgow", (55.9, -4.3), [7,5,3,0,6,5,0]),
            ("Plymouth", (50.4, -4.1), [4,9,0,0,0,6,5]),
            ("Aberdeen", (57.1, -2.1), [0,0,6,5,8,2,0]),
            ("Stornoway", (58.2, -6.4), [15,6,15,0,0,4,2]),
            ("Lerwick", (60.2, -1.1), [8,10,5,5,0,0,3]),
            ("St Helier", (49.2, -2.1), [0,0,0,0,6,10,0])]


--
getNames :: [Place] -> [String]
getNames place = [names | (names, _, _) <- place]

getNames2 :: [Place] -> String
getNames2 place = unlines [show(name) | (name, _,_) <- place]

getName :: Place -> String
getName (a, _, _) = a

getLocation :: Place -> (Float, Float)
getLocation (_, b, _) = b

getRainfall :: Place -> [Int]
getRainfall (_, _, c) = c

getPlaceFrom :: [Place] -> String -> Place
getPlaceFrom place x = p
  where (p:ps) = [y |  y <- place, getName y == x]

getAverageRainfall :: Place -> Float
getAverageRainfall place = fromIntegral (sum(getRainfall place)) / 7
  

getPlaceAverage :: [Place] -> String -> Float
getPlaceAverage place name = getAverageRainfall (getPlaceFrom place name)

getNameandRain :: [Place] -> String
getNameandRain place = unlines [show(name, rain) | (name, _,rain) <- place]
-- putStrLn(getNameandRain testData)

tableRainFormat :: Int -> Char -> String -> String
tableRainFormat number char string = replicate ( number - length string) char ++ string
tableLocationFormat :: Int -> Char -> String -> String
tableLocationFormat number char string = string ++ replicate ( number - length string) char

singlePlaceString :: Place -> String
singlePlaceString (name, _, rain) = tableLocationFormat 10  ' ' name ++ intercalate " | " (map(tableRainFormat 3 ' ' . show)rain)

placesToString :: [Place] -> String
placesToString = concat . intersperse "\n" . map singlePlaceString

--dryPlaces :: [Place] -> Int -> [String]
--dryPlaces testData x = [(name) | (name ,_ ,rain ) <- testData, rain!!x == 0]

dryPlaces :: [Place] -> Int -> [String]
dryPlaces place x = [(name) | (name ,_ ,rain ) <- place, rain!!(x-1) == 0]


updateRainfall :: [Place] -> [Int] -> [Place]
updateRainfall  _ [] = []
updateRainfall ((name, (geoRef), rainfall):place) (x:xs) =
    ((name, geoRef, x: init rainfall):updateRainfall place xs)

replaceRainfallData :: String -> Place -> [Place] -> [Place]
replaceRainfallData old new [] = []
replaceRainfallData old new ((location,geoRef,rainfall):place)
    | old == location = new : replaceRainfallData old new place
    | otherwise   = (location,geoRef,rainfall) : replaceRainfallData old new place
--replaceRainfallData "Plymouth" ("Portsmouth", (50.8,-1.1), [0, 0, 3, 2, 5, 2, 1]) testData

distToPlace :: (Float,Float) -> (Float,Float) -> Float
distToPlace (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

placesDryYesterday :: Place -> Bool
placesDryYesterday (_, _, r) = r !!0 == 0

closestDryPlace :: (Float,Float) -> [Place] -> Place
closestDryPlace (x,y)  = (minimumBy (comparing (distance (x, y)))) . filter placesDryYesterday

distance :: (Float,Float) -> Place -> Float
distance (x, y) (_, (x', y'), _) = distToPlace (x, y) (x',y')

nameOfPlace :: Place -> String
nameOfPlace (s,_, _) = s

closestPlaceName :: (Float, Float) -> [Place] -> String
closestPlaceName (x, y)  = nameOfPlace . closestDryPlace (x, y)




--
--  Demo
--

demo :: Int -> IO ()
demo 1  = print (getNames testData) -- display the names of all the places
demo 2 = printf "%.2f\n" (getPlaceAverage testData "Cardiff") -- display, to two decimal places, the average rainfall in Cardiff
demo 3 = putStrLn (placesToString testData)
demo 4 =  print (dryPlaces testData 2) -- display the names of all places that were dry two days ago
demo 5 = print (updateRainfall testData [0,8,0,0,5,0,0,3,4,2,0,8,0,0]) -- update the data with most recent rainfall
                                                                      --[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
demo 6 = print (replaceRainfallData "Plymouth" ("Portsmouth", (50.8,-1.1), [0, 0, 3, 2, 5, 2, 1]) testData)
-- replace "Plymouth" with "Portsmouth" which has
--          -- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
demo 7 = putStrLn (closestPlaceName (50.9,-1.3) testData)
--  -- demo 8 = -- display the rainfall map


--
-- Screen Utilities (use these to do the rainfall map - note that these do
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStrLn text


--
-- Your rainfall map code goes here
--
toInt :: (Float, Float) -> (Int, Int)
toInt (x, y) = (round x, round y)

rainfallMap :: [Place] ->IO ()
rainfallMap (x:xs) = writeAt(getLocation(toInt(x)) singlePlaceString(x))


--
-- Your user interface (and loading/saving) code goes here
--

-- Displays the main menu of actions to be chosen from.

main :: IO ()
main = do
  placeDataset <- loadPlaces
  putStrLn "The imported locational Datasets:\n"
  putStrLn (placesToString placeDataset)
  startMenu placeDataset


loadPlaces :: IO [Place]
loadPlaces = do
  contents <- readFile "data.txt"
  return (read contents :: [Place])

startMenu :: [Place] -> IO ()
startMenu placeDataset = do
  putStrLn ""
  putStrLn "Enter the number of the option desired: "
  putStrLn "1. Display all of the location names in the Dataset."
  putStrLn "2. Get the average raingfall from one of the above Dataset's"
  putStrLn "3"
  putStrLn "4"
  putStrLn "5. Update rainfall figures"
  putStrLn "6. Replace an existing place"
  putStrLn "7. Show the closest dry place from a given point"
  putStrLn "8. Display map"
  putStrLn "0. Exit"
  option <- getLine
  executeOption option placeDataset


executeOption :: String -> [Place] -> IO ()
executeOption "1" placeDataset = do
    putStrLn (getNames2 placeDataset)
    startMenu placeDataset
executeOption "2" placeDataset = do
  putStrLn "Get the Average from where?"
  userLocation <- getLine
  let avgcalc = printf "%.2f\n" (getPlaceAverage placeDataset userLocation)
  putStrLn ("The rainfall average for " ++  userLocation ++ " is "++ avgcalc)
  startMenu placeDataset
executeOption "3" placeDataset = do
  putStrLn ("Here is a nicely formatted table of our datasets!")
  putStrLn (placesToString placeDataset)
  startMenu placeDataset
executeOption "4" placeDataset = do
  putStrLn "Enter the number of days ago:"
  numDaysInput <- getLine
  let numDays = read numDaysInput :: Int
  print (dryPlaces placeDataset numDays)
  startMenu placeDataset
executeOption "5" placeDataset = do
  putStrLn "Enter the data to update:"
  dataInput <- getLine
  let newData = read dataInput :: [Int]
  let newPlaceDataset = (updateRainfall placeDataset newData)
  let placeDataset = newPlaceDataset
  putStrLn "Rainfall data successfully updated"
  startMenu placeDataset
executeOption "6" placeDataset = do
  putStrLn "Enter the name of the place you would like to replace:"
  oldPlace <- getLine
  putStrLn "Enter the new place data: "
  newPlaceInput <- getLine
  let newPlace = read newPlaceInput :: Place
  let newPlaceDataset = replaceRainfallData oldPlace newPlace placeDataset
  let placeDataset = newPlaceDataset
  putStrLn "Data updated sucessfully"
  startMenu placeDataset
executeOption "7" placeDataset = do
  putStrLn "Enter co-ordinates: "
  coordInput <- getLine
  let coordinates = read coordInput :: (Float, Float)
  putStrLn (closestPlaceName coordinates placeDataset)
  startMenu placeDataset

executeOption "0" placeDataset = do
  writeFile "data.txt" (show placeDataset)
executeOption _ placeDataset = do
  putStrLn "Select a valid option."
  startMenu placeDataset