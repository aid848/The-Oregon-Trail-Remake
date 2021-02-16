-- Main file for game setup
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import ScreenHandler
import Definitions


windowDisplay :: Display
windowDisplay = InWindow "Window" windowDims zoomDims

initialWorld :: World

initialWorld = World 0 0 [100,100,100,100,100] [[],[],[],[],[]] 0 (Node ([],[]) 0 "") 0 0 [] 0 "" 0 0 0  textTest


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
drawingFunc w = drawScreen w w -- todo fix this to not need the same argument twice 

-- A function to handle input events.
inputHandler :: Event -> World -> World -- todo case analysis here based on world state
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) w = w {message = "hihi"}
inputHandler _ w = w

-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced. 
updateFunc :: Float -> World -> World 
updateFunc _ w = w -- todo have set of actions here?
