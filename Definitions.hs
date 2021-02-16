module Definitions where
data Node = Node {
  next :: ([Node],[Int]),
  distance :: Int,
  shop :: String
}
data World = World {
    day :: Int,
    month :: Int,
    partyHealth :: [Int],
    partyConditions :: [String],
    food :: Int,
    nextLocation :: Node,
    position :: Int,
    cash :: Float,
    supplies :: [Int],
    rationing :: Float, -- percentage
    screenType :: String,
    pace :: Float, -- percentage
    oxen :: Int,
    userInput :: String, -- used for keyboard input of text like names
    userEnter :: Bool, -- used to confirm action
    userstage :: Int, -- used to show stage in screen
    -- todo remove
    health :: Integer,
    message :: String
}

initialWorld :: World
initialWorld = World 0 0 [100,100,100,100,100] [[],[],[],[],[]] 0 (Node ([],[]) 0 "") 0 0 [] 0 "" 0 0 "" False 0 0  "textTest"

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