
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
-- =========================Task i==============================================
getNames :: [Place] -> [String]
getNames place = [names | (names, _, _) <- place]

getNames2 :: [Place] -> String
getNames2 place = unlines [show(name) | (name, _,_) <- place]

-- =========================Task ii=============================================
getRainfall :: Place -> [Int]
getRainfall (_, _, c) = c

getName :: Place -> String
getName (a, _, _) = a

getPlaceFrom :: [Place] -> String -> Place
getPlaceFrom place x = p
  where (p:ps) = [y |  y <- place, getName y == x]

getAverageRainfall :: Place -> Float
getAverageRainfall place = fromIntegral (sum(getRainfall place)) / 7

getPlaceAverage :: [Place] -> String -> Float
getPlaceAverage place name = getAverageRainfall (getPlaceFrom place name)

-- =========================Task iii============================================

tableRainFormat :: Int -> Char -> String -> String
tableRainFormat number char string = replicate ( number - length string) char ++ string
tableLocationFormat :: Int -> Char -> String -> String
tableLocationFormat number char string = string ++ replicate ( number - length string) char

singlePlaceString :: Place -> String
singlePlaceString (name, _, rain) = tableLocationFormat 10  ' ' name ++ intercalate " | " (map(tableRainFormat 3 ' ' . show)rain)

placesToString :: [Place] -> String
placesToString = concat . intersperse "\n" . map singlePlaceString

-- =========================Task iv=============================================
dryPlaces :: [Place] -> Int -> [String]
dryPlaces place x = [(name) | (name ,_ ,rain ) <- place, rain!!(x-1) == 0]

-- =========================Task v==============================================
updateRainfall :: [Place] -> [Int] -> [Place]
updateRainfall  _ [] = []
updateRainfall ((name, (geoRef), rainfall):place) (x:xs) =
    ((name, geoRef, x: init rainfall):updateRainfall place xs)

-- =========================Task vi=============================================
replaceRainfallData :: String -> Place -> [Place] -> [Place]
replaceRainfallData old new [] = []
replaceRainfallData old new ((location,geoRef,rainfall):place)
    | old == location = new : replaceRainfallData old new place
    | otherwise   = (location,geoRef,rainfall) : replaceRainfallData old new place

-- =========================Task vii============================================
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
demo 8 = rainfallMap testData


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
    putStr text

--
-- Your rainfall map code goes here
-- Rainfall map
--
rainfallMap :: [Place] -> IO ()
rainfallMap place = do
    clearScreen
    plotPlaces place 0 (getScalingBounds place)
    goTo (0,0)

-- Takes the arr of places, the current index, the bounds to scale from
plotPlaces :: [Place] -> Int -> (Float, Float, Float, Float) -> IO ()
plotPlaces place currIndex bounds = do
    -- If we are at the last index then do nothing else move up to next
    if length place - 1 == currIndex then return () else
        plotPlaces place (currIndex + 1) bounds
    plot (place !! currIndex) bounds

averageIntsRounded :: [Int] -> Float
averageIntsRounded a = rnd $ fromIntegral (sum a) / fromIntegral (genericLength a)
    -- rnd rounds a float to 2 decimal places
    where rnd f = (/100) $ fromIntegral $ round (f * 100)

-- Takes a place and the bounds to scale from, shows place on screen with scaled coords
plot :: Place -> (Float, Float, Float, Float) -> IO ()
plot (name,(y,x),rain) bounds = do
    -- 50- to flip y axis
    writeAt (scaledX, 50-scaledY) "+"
    writeAt (scaledX + 2, 50-scaledY) name
    writeAt (scaledX + length name + 3 , 50-scaledY) (show (averageIntsRounded rain))
    where
        (scaledX, scaledY) = scale bounds x y

scale :: (Float, Float, Float, Float) -> Float -> Float -> (Int, Int)
scale (minX, maxX, minY, maxY) currX currY = (round scaledX, round scaledY)
    where
        -- I don't fully understand the logic behind the maths, but it works :)
        -- The width is lowered from 80 to 65 so the text and avg wont overflow the line
        scaledX = (currX - minX) * 65 / (maxX - minX)
        scaledY = (currY - minY) * 50 / (maxY - minY)


getScalingBounds :: [Place] -> (Float, Float,Float, Float)
getScalingBounds place =
    (placeX (minimumBy sortByXValue place),
     placeX (maximumBy sortByXValue place),
     placeY (minimumBy sortByYValue place),
     placeY (maximumBy sortByYValue place))
        where
        sortByXValue (_,(_,x1),_) (_,(_,x2),_)
            | x1 < x2 = LT
            | x1 > x2 = GT
            | otherwise = EQ
        sortByYValue ( _, (y1,_),_) ( _, (y2,_), _)
            | y1 < y2 = LT
            | y1 > y2 = GT
            | otherwise = EQ
        placeX ( _,(_,x), _) = x
        placeY ( _, (y,_), _) = y




-- Displays the main menu of actions to be chosen from.


--HelperFunc
checkLocExists :: String -> [Place] -> Bool
checkLocExists name ((n, _, _):xs)
 | n == name = True
 | xs == [] = False
 | otherwise = checkLocExists name xs


--
-- Your user interface (and loading/saving) code goes here
--
main :: IO ()
main = do 
 putStrLn("========================================================")
 putStrLn("Rain Dataset")
 loadedFile <- readFile "data.txt"
 let rainDataset = read loadedFile
 putStrLn("\nSuccesfully loaded "++ show(length rainDataset) ++" locations!")
 putStrLn("========================================================\n")
 inMainMenu rainDataset

inMainMenu ::[Place] -> IO()
inMainMenu rainDataset = do 
 putStrLn("========================================================")
 putStrLn "Below are the availible options: "
 putStrLn "1. Display all of the location names in the Dataset."
 putStrLn "2. Get the average raingfall from one of the above Dataset's"
 putStrLn "3. Output a nicely formatted table of the loaded datasets"
 putStrLn "4. display the names of all places that were dry *n* days ago (n=1-7)"
 putStrLn "5. Update rainfall figures"
 putStrLn "6. Replace an existing place"
 putStrLn "7. Show the closest dry place from a given point"
 putStrLn "8. Display map"
 putStrLn "0. Exit"
 putStrLn("========================================================\n")
 putStr("Please insert your option: ")
 option <- getLine
 putStrLn("\n")
 inAction option rainDataset

inAction :: String -> [Place] -> IO ()
-- Save the database and exit the UI
inAction "0" rainDataset = pickedChoice "0" rainDataset
-- Display all of the location names in the Dataset.
inAction "1" rainDataset = pickedChoice "1"  rainDataset
-- Display all the films within the database
inAction "2" rainDataset = pickedChoice "2" rainDataset
-- -- Get all the films that were released after a certain date
inAction "3" rainDataset = pickedChoice "3"  rainDataset
-- -- Get all the films with a particular fan
inAction "4" rainDataset = pickedChoice "4"  rainDataset
-- -- Get all the fans of a particular film
inAction "5" rainDataset = pickedChoice "5"  rainDataset
-- -- Assign a fan to a particular film
inAction "6" rainDataset = pickedChoice "6" rainDataset
-- -- All fans of films directed by a particular director
inAction "7" rainDataset = pickedChoice "7" rainDataset
-- -- All directors & no. of their films that a particular fan is is a fan of
inAction "8" rainDataset = pickedChoice "8" rainDataset
-- -- Display errors if the input is invalid!
inAction _ rainDataset = do
 putStrLn (inSendErrorInput "option" )
 inMainMenu rainDataset

inSendErrorInput :: String -> String
inSendErrorInput "int" = "\n[Error]: The value entered was not the expected value (Number / Int)\n[Error]: Option is being reset, please try again.\n"
inSendErrorInput "string" = "\n[Error]: The value entered was not the expected value (Word / String)\n[Error]: Option is being reset, please try again.\n"
inSendErrorInput "input" = "\n[Error]: There was no input given!\n[Error]: Option is being reset, please try again.\n"
inSendErrorInput "year" = "\n[Error]: The year is not valid, enter a value after year 1900 - 2100.!\n[Error]: Option is being reset, please try again.\n"
inSendErrorInput "option" = "\n[Error]: Your input is not valid. Select option 1-7 or 0 to save and exit\n"
inSendErrorInput _ = ""


pickedChoice :: String -> [Place] -> IO ()
pickedChoice "1"  rainDataset = do  
    putStrLn ("Here is a list of our datasets!")
    print (getNames rainDataset)
    inMainMenu rainDataset
pickedChoice "2" rainDataset = do
  putStrLn "Get the Average from where?"
  userLocation <- getLine
  case (checkLocExists userLocation rainDataset) of
    True -> do     
        let avgcalc = printf "%.2f\n" (getPlaceAverage rainDataset userLocation)
        putStrLn ("The rainfall average for " ++  userLocation ++ " is "++ avgcalc)
        inMainMenu rainDataset
    False -> do 
        putStrLn ("location not valid")
        pickedChoice "2" rainDataset
  inMainMenu rainDataset
pickedChoice "3"  rainDataset = do
  putStrLn ("Here is a nicely formatted table of our datasets!")
  putStrLn (placesToString  rainDataset)
  inMainMenu rainDataset
pickedChoice "4" rainDataset = do
  putStrLn "Enter the number of days ago:"
  numDaysInput <- getLine
  let numDays = read numDaysInput :: Int
  case (numDays >= 1 && numDays <= 7) of
    True -> do
      print (dryPlaces rainDataset numDays)
      inMainMenu rainDataset
    False -> do
      putStrLn("shut up bruv")
      pickedChoice "4"  rainDataset
pickedChoice "5" rainDataset = do
  putStrLn "Enter the data to update:"
  dataInput <- getLine
  let newData = read dataInput :: [Int]
  case (length dataInput <= 14) of
    True -> do
      let newRainDataset = (updateRainfall rainDataset newData)
      let rainDataset = newRainDataset
      putStrLn "Rainfall data successfully updated"
      inMainMenu rainDataset
    False ->  do
      putStrLn("nae working")
      pickedChoice "5"  rainDataset
pickedChoice "6" rainDataset = do
  putStrLn "Enter the name of the place you would like to replace:"
  oldPlace <- getLine
  case (checkLocExists oldPlace rainDataset) of
    True -> do
      putStrLn "Enter the new place data: "
      newPlaceInput <- getLine
      let newPlace = read newPlaceInput :: Place
      let newRainDataset = replaceRainfallData oldPlace newPlace rainDataset
      let rainDataset = newRainDataset
      putStrLn "Data updated sucessfully"
      inMainMenu rainDataset
    False -> do
      putStrLn ("place does not exit")
      pickedChoice "6" rainDataset
pickedChoice "7" rainDataset = do
  putStrLn "Enter co-ordinates: "
  coordInput <- getLine
  let coordinates = read coordInput :: (Float, Float)
  putStrLn (closestPlaceName coordinates rainDataset)
  inMainMenu rainDataset
pickedChoice "8" rainDataset = do
  rainfallMap rainDataset
pickedChoice "0"  rainDataset = do
  writeFile "data.txt" (show  rainDataset)
pickedChoice _  rainDataset = do
  putStrLn "Select a valid option."
  inMainMenu  rainDataset
pickedChoice "0"  rainDataset = do
  writeFile "data.txt" (show  rainDataset)
pickedChoice _  rainDataset = do
  putStrLn "Select a valid option."
  inMainMenu  rainDataset