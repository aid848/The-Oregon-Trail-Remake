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
inputHandler (EventKey (Char '0')  Down _ _) w = handleKey0 w
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
inputHandler (EventKey (Char 'a')  Down _ _) w = handleCharA w
inputHandler (EventKey (Char 'b')  Down _ _) w = handleCharB w
inputHandler (EventKey (Char 'c')  Down _ _) w = handleCharC w
inputHandler (EventKey (Char 'd')  Down _ _) w = handleCharD w
inputHandler (EventKey (Char 'e')  Down _ _) w = handleCharE w
inputHandler (EventKey (Char 'f')  Down _ _) w = handleCharF w
inputHandler (EventKey (Char 'g')  Down _ _) w = handleCharG w
inputHandler (EventKey (Char 'h')  Down _ _) w = handleCharH w
inputHandler (EventKey (Char 'i')  Down _ _) w = handleCharI w
inputHandler (EventKey (Char 'j')  Down _ _) w = handleCharJ w
inputHandler (EventKey (Char 'k')  Down _ _) w = handleCharK w
inputHandler (EventKey (Char 'l')  Down _ _) w = handleCharL w
inputHandler (EventKey (Char 'm')  Down _ _) w = handleCharM w
inputHandler (EventKey (Char 'o')  Down _ _) w = handleCharO w
inputHandler (EventKey (Char 'p')  Down _ _) w = handleCharP w
inputHandler (EventKey (Char 'q')  Down _ _) w = handleCharQ w
inputHandler (EventKey (Char 'r')  Down _ _) w = handleCharR w
inputHandler (EventKey (Char 's')  Down _ _) w = handleCharS w
inputHandler (EventKey (Char 't')  Down _ _) w = handleCharT w
inputHandler (EventKey (Char 'u')  Down _ _) w = handleCharU w
inputHandler (EventKey (Char 'v')  Down _ _) w = handleCharV w
inputHandler (EventKey (Char 'w')  Down _ _) w = handleCharW w
inputHandler (EventKey (Char 'x')  Down _ _) w = handleCharX w
inputHandler (EventKey (Char 'z')  Down _ _) w = handleCharZ w
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
