-- Main file for game setup
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Test
import ScreenHandler
import Definitions



-- data Node = Node {
--   next :: ([Node],[Int]),
--   distance :: Int,
--   shop :: String
-- }

-- data World = World {
--     day :: Int,
--     month :: Int,
--     partyHealth :: [Int],
--     partyConditions :: [String],
--     food :: Int,
--     nextLocation :: Node,
--     position :: Int,
--     cash :: Float,
--     supplies :: [Int],
--     rationing :: Float, -- percentage
--     screenType :: String,
--     pace :: Float, -- percentage
--     oxen :: Int,
--     -- todo remove
--     health :: Integer,
--     message :: String
-- }

windowDisplay :: Display
windowDisplay = InWindow "Window" (1280, 720) (0, 0)

initialWorld :: World
initialWorld = World 0 0 [] [] 0 (Node ([],[]) 0 "") 0 0 [] 0 "" 0 0 0  textTest

main :: IO ()
main = play
  windowDisplay -- Display mode. 
  black -- Background color. 
  1 -- Number of simulation steps to take for each second of real time. (fps?)
  initialWorld
  drawingFunc
  inputHandler
  updateFunc
    

drawingFunc :: World -> Picture -- A function to convert the world a picture.
drawingFunc w = drawScreen w

-- A function to handle input events.
inputHandler :: Event -> World -> World -- todo case analysis here based on world state
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) w = w {message = "hihi"}
inputHandler _ w = w

-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced. 
updateFunc :: Float -> World -> World 
updateFunc _ w = w -- todo have set of actions here?
