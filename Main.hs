-- Main file for game setup
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import ScreenHandler
import Definitions
import UpdateHandler
import KeyHandler


windowDisplay :: Display
windowDisplay = InWindow "Window" windowDims zoomDims


main :: IO ()
main = do
  shopguy <- loadBMP "shop.bmp"
  wagon <- loadBMP "wagon.bmp"
  wagon2 <- loadBMP "wagon2.bmp"
  map <- loadBMP "map.bmp"
  play windowDisplay black 1 (initialWorld [shopguy,wagon,map,wagon2]) drawingFunc inputHandler updateFunc
    

drawingFunc :: World -> Picture -- A function to convert the world a picture.
drawingFunc w = drawScreen w w  -- todo fix this to not need the same argument twice 

-- A function to handle input events.
inputHandler :: Event -> World -> World -- todo case analysis here based on world state
inputHandler (EventKey (Char '1')  Down _ _) w = handleKey1 w
inputHandler (EventKey (Char '2')  Down _ _) w = handleKey2 w
inputHandler (EventKey (Char '3')  Down _ _) w = handleKey3 w
inputHandler (EventKey (Char '4')  Down _ _) w = handleKey4 w
inputHandler (EventKey (Char '5')  Down _ _) w = handleKey5 w
inputHandler (EventKey (Char '6')  Down _ _) w = handleKey6 w
inputHandler (EventKey (Char '7')  Down _ _) w = handleKey7 w
inputHandler (EventKey (Char '8')  Down _ _) w = handleKey8 w
inputHandler (EventKey (Char '9')  Down _ _) w = handleKey9 w
inputHandler (EventKey (Char 'y')  Down _ _) w = handleCharY w
inputHandler (EventKey (Char 'n')  Down _ _) w = handleCharN w
inputHandler (EventKey (SpecialKey KeySpace) Down _ _) w = handleSpaceKey w
inputHandler (EventKey (SpecialKey KeyEnter) Down _ _) w = handleEnterKey w
inputHandler (EventKey (SpecialKey KeyCtrlL) Down _ _) w = handleCtlKey w
inputHandler (EventKey (SpecialKey KeyCtrlR) Down _ _) w = handleCtlKey w
inputHandler _ w =  w

-- A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced. 
updateFunc :: Float -> World -> World 
updateFunc _ w
  | screenType w == "On route" && userstage w == 0 = update w
  | otherwise                                          = w
