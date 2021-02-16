module ScreenHandler where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Definitions
import TextElements
textColor = white

-- top level screen drawer based on world state
drawScreen :: World -> World -> Picture
drawScreen World{screenType=""} w = shopScreen -- for testing, remove or keep for showing error
drawScreen World{screenType="Start"} w = startScreen
drawScreen World{screenType="On route"} w = onRouteScreen
drawScreen World{screenType="Shop"} w = shopScreen
drawScreen World{screenType="Settlement"} w = settlementScreen
drawScreen World{screenType="River"} w = riverScreen
drawScreen World{screenType="Inventory"} w = inventoryScreen
drawScreen World{screenType="Generic"} w = genericScreen
drawScreen World{screenType="Splash"} w = splashScreen


-- outputs a Picture of the input text to allow for text wrapping and not drawing off the screen
-- supporting only full width and half width so far
textWriter :: String -> String -> Picture
textWriter str "full" = arrangeText (foldr (\ x y -> (Scale 0.25 0.25(Text x)):y) [] (splitText str fullTextSpan))
textWriter str _ = arrangeText (foldr (\ x y -> (Scale 0.25 0.25(Text x)):y) [] (splitText str halfTextSpan))

--todo have another textWriter for none automatic string splitting for static elements like menus
 
-- aranages a block of text pictures so that the resulting Picture doesn't overlap 
arrangeText :: [Picture] -> Picture
arrangeText lst = Pictures (map (\(x,y) ->  Translate (0) (-50*y) (x) ) (zip lst [0..]))


-- todo cut out spaces at the edges and add hyphen to words that will be split into different lines
-- splits text into sub strings due to limited screen space
splitText :: String -> Int -> [String]
splitText [] _ = []
splitText str len = (take len str):(splitText (drop len str) len)


-- calculates screen coord location for a picture
anchorElement :: String -> Picture -> Picture
anchorElement "top full text" pic = Translate (halfTextSpanF - halfX) (halfY - textHeightF) pic -- text on top half of screen
anchorElement "mid" pic = Translate 0 0 pic -- already centered by default
anchorElement "bottom full text" pic = Translate (halfTextSpanF - halfX) (-textHeightF) pic -- text on bottom half of screen
anchorElement "top half text" pic = Translate (halfTextSpanF - halfX/4) (halfY - textHeightF) pic

-- fetches user input text
userText :: World -> String
userText w = userInput w

-- rectangle generator, generates an x-y path for a gloss polygon TODO
-- generateRect :: Float -> Float -> Path
-- generateRect = 

-- retrieve bitmap data for rendering on screen TODO
-- drawBitmap :: ? -> Picture
-- drawBitmap = 

-- ********************** Screen definitions **********************
-- pattern: array of elements -> picture -> combine into one large picture

-- splash screen todo (if time)
splashScreen = Color white ( anchorElement "bottom full text" (textWriter "bigTxt" "full"))

-- used to test output
testScreen = Color white ( anchorElement "bottom full text" (textWriter "bigTxt" "full"))


-- starting screen todo
startScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- On route todo
onRouteScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- Shop TODO
shopTextElements = Pictures [ Color red (anchorElement "top half text" (textWriter generalStoreHeader "half"))]
-- shopBars = Pictures [ Color red (anchorElement "top full text" (textWriter generalStoreHeader "half"))]
shopScreen = Pictures [shopTextElements]

-- Settlement TODO
settlementScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- River TODO 
riverScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- Inventory TODO 
inventoryScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- Generic menu (for changing settings and stuff) TODO sus on this one
genericScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- ******************* End of screen definitions *******************