module Definitions where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Date
import Map



data World = World {
    date :: Date,
    partyNames :: [String],
    partyHealth :: [Int], -- length=5, each party member is one of: 0=dead, [1..25]=very poor, [26..50]=poor, [51..75]=fair, [76..100]=healthy
    partyConditions :: [[String]], -- each member can be one or combination of "cholera", "dysentery" "measles", "fever" or []
    partyIsDead :: [Bool], -- length=5, each party member is one of True=dead, False=alive
    food :: Int,
    clothing :: Int,
    medicine :: Int,
    parts :: Int,
    nextLocation :: Node,           -- Either next in currentLocation node, or result of user selection if currentLocation had a branch
    currentLocation :: Node,        -- Node that we are either in, or in transit from
    milesTravelled :: Int,          -- cumulative distance travelled
    cash :: Float,
    buildBill :: Float,
    bill :: Float,
    rationing :: Int, -- 1 = filling, 2 = meager, 3 = bare bones
    screenType :: String,
    pace :: Int, -- 1 = steady, 2 = strenuous, 3 = grueling
    oxen :: Int,
    userInput :: String, -- used for keyboard input of text like names
    userEnter :: Bool, -- used to confirm action
    userstage :: Int, -- used to show stage in screen
    message :: String,
    weather :: String,
    rngSeed :: Int, -- used to generate random numbers 
    cart :: [(String, Int, Float)],  -- e.g. [("Oxen", 2, 320.00)]
    imgs :: [Picture]
} deriving(Show)


startingDate :: Date
startingDate = dateCons 1 "March" 1848

initialWorld :: [Picture] -> World
initialWorld i = World startingDate ["", "", "", "", ""] [100,100,100,100,100] [[],[],[],[],[]] [False, False, False, False, False] 0 0 0 0 fortKearney independence  0 0.0 0.0 0.0 1 "Start" 1 0 "" False 0  "" "Cloudy" 42 [] i

nWorld :: World -> World
nWorld w = World startingDate ["", "", "", "", ""] [100,100,100,100,100] [[],[],[],[],[]] [False, False, False, False, False] 0 0 0 0 fortKearney independence  0 0.0 0.0 0.0 1 "Start" 1 0 "" False 0  "" "Cloudy" 42 [] (imgs w)

windowDims :: (Int,Int)
windowDims = (1280, 720)

xDim :: Float
xDim = 1280

yDim :: Float
yDim = 720

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