module Definitions where
import Date
import Map



data World = World {
    date :: Date,
    partyHealth :: [Int],
    partyConditions :: [[String]], -- 0 = dead, 1 = poor, 2 = fair, 3 = good
    food :: Int,
    nextLocation :: Node,
    position :: Int,
    cash :: Float,
    supplies :: [Int],
    rationing :: Int, -- 1 = filling, 2 = meager, 3 = bare bones
    screenType :: String,
    pace :: Int, -- 1 = steady, 2 = strenuous, 3 = grueling
    oxen :: Int,
    userInput :: String, -- used for keyboard input of text like names
    userEnter :: Bool, -- used to confirm action
    userstage :: Int, -- used to show stage in screen
    message :: String 
}

-- startingShop 

startingDate :: Date
startingDate = dateCons 1 "March" 1848

initialWorld :: World
initialWorld = World startingDate [100,100,100,100,100] [[],[],[],[],[]] 0 (buffalo_head) 0 0 [] 0 "" 0 0 "" False 0  "textTest"

windowDims :: (Int,Int)
windowDims = (1280, 720)

halfX :: Float
halfX = 1280 / 2

halfY :: Float
halfY = 720 / 2

zoomDims :: (Int,Int)
zoomDims = (0, 0)

fullTextSpan :: Int
fullTextSpan = 70

halfTextSpan :: Int
halfTextSpan = 35

textHeight :: Int
textHeight = 50

halfTextSpanF :: Float
halfTextSpanF = 35.0

textHeightF :: Float
textHeightF = 50.0