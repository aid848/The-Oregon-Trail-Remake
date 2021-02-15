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
    -- todo remove
    health :: Integer,
    message :: String
}
