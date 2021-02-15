module ScreenHandler where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Definitions
import Test
textColor = white

drawScreen :: World -> World -> Picture -- TODO switch on world screenType here and use appropriate screen builder
drawScreen World{screenType=""} w = testScreen
drawScreen World{screenType="Initialization"} w = Color green (Pictures [Translate (-600) (300) (Scale 0.25 0.25 (Text (message w))),
 Translate (-600) (250) (Scale 0.25 0.25 (Text (message w)))] )

-- todo add more cases :
-- On route
-- Shop
-- Settlement
-- River
-- Inventory
-- Generic menu (for changing settings and stuff)



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


-- calculates screen coord location for a picture TODO
-- anchorElement :: String -> Picture -> Picture
-- anchorElement "left" = 0
-- anchorElement "right" = 0
-- anchorElement "top" = 0
-- anchorElement "mid" = 0
-- anchorElement "bottom" = 0

-- ********************** Screen definitions **********************

-- used to test output
testScreen = Color white (Translate (-1280/2 + 35) (720/2 - 50) (textWriter bigTxt "full"))

-- splash screen todo (if time)

-- starting screen todo
startScreen = 0

-- todo add more cases :
-- On route
-- Shop
-- Settlement
-- River
-- Inventory
-- Generic menu (for changing settings and stuff)


-- ******************* End of screen definitions *******************