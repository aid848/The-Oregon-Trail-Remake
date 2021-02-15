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
    -- todo remove
    health :: Integer,
    message :: String
}

windowDims :: (Int,Int)
windowDims = (1280, 720)

zoomDims :: (Int,Int)
zoomDims = (0, 0)

fullTextSpan :: Int
fullTextSpan = 70

halfTextSpan :: Int
halfTextSpan = 35

textHeight :: Int
textHeight = 50