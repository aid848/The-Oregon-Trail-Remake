-- Always imported
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Test

data World = World {
    health :: Integer,
    message :: String
}

windowDisplay :: Display
windowDisplay = InWindow "Window" (1280, 720) (0, 0)

initialWorld :: World
initialWorld = World 5 textTest

main :: IO ()
main = play
  windowDisplay -- Display mode. 
  white -- Background color. 
  1 -- Number of simulation steps to take for each second of real time. (fps?)
  initialWorld
  drawingFunc
  inputHandler
  updateFunc
    

drawingFunc :: World -> Picture -- A function to convert the world a picture.
drawingFunc w = Pictures [Translate (-600) (300) (Scale 0.25 0.25 (Text (message w))),
 Translate (-600) (250) (Scale 0.25 0.25 (Text (message w)))]

-- A function to handle input events.
inputHandler :: Event -> World -> World -- todo case analysis here based on world state
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) w = w {message = "hihi"}
inputHandler _ w = w

-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced. 
updateFunc :: Float -> World -> World 
updateFunc _ w = w -- todo have set of actions here?
