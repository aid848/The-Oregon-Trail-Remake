module ScreenHandler where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Definitions
import TextElements
import Date
import Shop
import Map

textColor = white

-- top level screen drawer based on world state
drawScreen :: World -> World -> Picture
drawScreen World{screenType=""} w = settlementScreen w w -- for testing, remove or keep for showing error
drawScreen World{screenType="Start"} w = startScreen
drawScreen World{screenType="On route"} w = onRouteScreen w w
drawScreen World{screenType="Shop"} w = shopScreen w w
drawScreen World{screenType="Settlement"} w = settlementScreen w w
drawScreen World{screenType="River"} w = riverScreen
drawScreen World{screenType="Inventory"} w = inventoryScreen w w
drawScreen World{screenType="Generic"} w = genericScreen
drawScreen World{screenType="Splash"} w = splashScreen


-- outputs a Picture of the input text to allow for text wrapping and not drawing off the screen
-- supporting only full width and half width so far
textWriter :: String -> String -> Picture
textWriter str "full" = Color white (arrangeText (foldr (\ x y -> (Scale 0.25 0.25(Text x)):y) [] (splitText str fullTextSpan)))
textWriter str _ = Color white (arrangeText (foldr (\ x y -> (Scale 0.25 0.25(Text x)):y) [] (splitText str halfTextSpan)))

textWriterInverted :: String -> String -> Picture
textWriterInverted str "full" = Color black (arrangeText (foldr (\ x y -> (Scale 0.25 0.25(Text x)):y) [] (splitText str fullTextSpan)))
textWriterInverted str _ = Color black (arrangeText (foldr (\ x y -> (Scale 0.25 0.25(Text x)):y) [] (splitText str halfTextSpan)))

--todo have another textWriter for none automatic string splitting for static elements like menus
textWriterFormatted :: [String] -> Picture
textWriterFormatted arr = Color white (arrangeText (foldr (\ x y -> (Scale 0.25 0.25(Text x)):y) [] arr))


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
anchorElement "bottom full text" pic = Translate (halfTextSpanF - halfX) (-textHeightF) pic -- text on bottom half of screen
anchorElement "top half text" pic = Translate (halfTextSpanF - halfX/4) (halfY - textHeightF) pic
anchorElement "bottom short half" pic = Translate (halfTextSpanF - halfX/2) (-halfY + textHeightF) pic 

-- fetches user input text
userText :: World -> String
userText w = userInput w

lineGen :: Float -> Float -> Picture
lineGen x y = (Polygon [(x/(-2),y/2),(x/(-2),y/(-2)),(x/2,y/(-2)),(x/2,y/2)])

partyHealthToWord :: [Int] -> String -- TODO
partyHealthToWord partyHp = "good"

-- 1 = steady, 2 = strenuous, 3 = grueling
paceToWord :: Int -> String -- TODO
paceToWord val = "steady"

dateText :: World -> String
dateText w = (month (date w))++" "++(show (day (date w)))++", "++(show (year (date w)))

weatherText :: World -> String
weatherText w = ("Weather: "++(weather w))

healthText :: World -> String
healthText w = ("Health: "++(partyHealthToWord(partyHealth w)))

foodText :: World -> String
foodText w = ("Food: "++(show (food w))++" pounds")

--todo change to remaining distance
landmarkText :: World -> String
landmarkText w = ("Next landmark: "++(show (dist (nextLocation w)))++" miles")

-- todo add this to world props or something
milesTraveledText :: World -> String
milesTraveledText w = ("Miles Traveled: "++"Todo"++" miles")

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

-- On route (stage 0 = stopped, stage 1 = traveling stage, 2 = stopped dialogue box)

routeStatusBackground:: Picture
routeStatusBackground = Color white (lineGen xDim 275)

routeDate:: World -> Picture
routeDate w = textWriterInverted (dateText w) "full"

routeWeather:: World -> Picture
routeWeather w = textWriterInverted (weatherText w) "full"

routeHealth:: World -> Picture
routeHealth w = textWriterInverted (healthText w) "full"

routeFood:: World -> Picture
routeFood w = textWriterInverted (foodText w) "full"

routeLandmark:: World -> Picture
routeLandmark w = textWriterInverted (landmarkText w) "full"

routeMilesTraveled:: World -> Picture
routeMilesTraveled w = textWriterInverted (milesTraveledText w) "full"

routeStatusBar :: World -> Picture
routeStatusBar w = Translate (0) (-225) (Pictures[routeStatusBackground,routeDate w, routeWeather w, routeHealth w, routeFood w,routeLandmark w, routeMilesTraveled w])

routeNearPlane :: Picture -- static for now but it could be different?
routeNearPlane = Color green (lineGen xDim 175)

-- todo draw a line with a sine wave or something for a far plane effect
routeFarPlane :: Picture
routeFarPlane = Text "Todo"

-- todo have message and spot for user input
routeDialogueBox :: World -> Picture
routeDialogueBox w = Text "Todo"

-- load bitmap
routeWagon :: Picture
routeWagon = Text "Todo"

-- gonna need some world props or something to do the animation of approching river
routeRiver :: World -> Picture
routeRiver w = Text "Todo"



onRouteScreen :: World -> World -> Picture
onRouteScreen World{userstage = 0} w = Color white (Pictures[routeNearPlane,routeStatusBar w])


-- Shop (userstage 0 = main shop menu, 1 = item selected and asking how much to buy and info about it)
-- TODO stage 1 with item info and amount to buy needed
-- TODO on shop user stage 0 set userInput to _
-- TODO fix money from d.c to d.cc
-- TODO change bill and prices to amount in shop based on selection

shopMoneyCount :: World -> Picture
shopMoneyCount w = Translate (-200) (-140) (textWriter ("Amount you have: $"++(show (cash w))) "half")

showItemSelect :: World -> Picture
showItemSelect w = Translate (-200) (-205) (textWriter ("Which item would you like to buy? "++((userInput w))) "half")

shopBill:: World -> Picture
shopBill w = Translate (125) (-75) (textWriter ("Total bill:  $"++((show (bill w)))) "half")

shopDate :: World -> Picture
shopDate w = Translate (0) (255) (textWriter ((month (date w))++" "++(show (day (date w)))++", "++(show (year (date w))) ) "half")

shopItemList :: World -> Picture
shopItemList w = textWriterFormatted (map (\(x,y) -> x) (items (shop (nextLocation w))))

shopPriceList :: World -> Picture
shopPriceList w = textWriterFormatted (map (\(x,y) -> "$"++(show y)) (items (shop (nextLocation w))))

-- in two parts: one for listing of items, one for prices so they stay aligned
shopStockShow :: World -> Picture
shopStockShow w = Translate (-200) (200) (Pictures [shopItemList w,Translate (500) (0) (shopPriceList w )])

shopLeaveMessage :: Picture
shopLeaveMessage = (anchorElement "bottom short half" (textWriter storeLeave "half"))

shopNameMessage:: World -> Picture
shopNameMessage w = (anchorElement "top half text" (textWriter ((store (shop (nextLocation w)))) "half"))

-- top to bottom arrangement of bars
shopBars :: Picture
shopBars = color red (Pictures [Translate (100) (350) (lineGen 700 10) , Translate (100) (235) (lineGen 700 10), Translate (100) (-25) (lineGen 700 10)])

shopStaticTextElements :: Picture
shopStaticTextElements = Pictures [shopLeaveMessage, shopBars]

shopDynamicElements :: World -> Picture
shopDynamicElements w = Pictures [shopMoneyCount w,showItemSelect w, shopBill w, shopDate w, shopStockShow w, shopNameMessage w]


shopScreen :: World -> World -> Picture
shopScreen World{userstage = 0} w = Pictures [shopStaticTextElements,(shopDynamicElements w)]
shopScreen World{userstage = 1} w = Pictures [shopStaticTextElements,(shopDynamicElements w)]

-- Settlement (user state based on selection number)

-- 1 = filling, 2 = meager, 3 = bare bones
rationsToWord :: Int -> String -- TODO
rationsToWord ra = "filling"

settleName :: World -> Picture
settleName w = anchorElement "top half text" (textWriter (name (nextLocation w)) "full")


settleDate :: World -> Picture
settleDate w = Translate (-125) (265) (textWriter (dateText w) "full")


settleWeather :: World -> Picture
settleWeather w = Translate (-350) (38) (textWriterInverted (weatherText w) "half")

settleHealth :: World -> Picture
settleHealth w = Translate (-350) (8) (textWriterInverted ("Health: "++(partyHealthToWord (partyHealth w))) "half")

settlePace :: World -> Picture
settlePace w = Translate (-350) (-22) (textWriterInverted ("Pace: "++paceToWord((pace w))) "half")

settleRations :: World -> Picture
settleRations w = Translate (-350) (-52) (textWriterInverted ("Rations: "++rationsToWord((rationing w))) "half")

settleStatusBar :: World -> Picture
settleStatusBar w = Translate (0) (175) (Pictures [(color white (lineGen 800 130)), settleWeather w, settleHealth w ,settlePace w, settleRations w ])

settleActions :: Picture
settleActions = Translate (-400) (75) (textWriterFormatted settleActionsText)

settleChoice :: World -> Picture
settleChoice w = Translate (50) (-325) (textWriter ("What is your choice? "++(userInput w)) "half")

-- TODO all the other screens for actions -_-
settlementScreen World{userstage = 0} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, settleChoice w]
-- settlementScreen World{userstage = 1} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, settleChoice w]

-- River TODO 
riverScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- Inventory TODO all the other screens for actions -_-
inventoryActions :: Picture
inventoryActions = Translate (-400) (75) (textWriterFormatted invActionsText)

inventoryScreen World{userstage = 0} w = Pictures [settleDate w,settleStatusBar w, inventoryActions, settleChoice w]

-- Generic menu (for changing settings and stuff) TODO sus on this one
genericScreen = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- ******************* End of screen definitions *******************