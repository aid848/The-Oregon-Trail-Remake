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
drawScreen World{screenType="Start"} w = startScreen w w
drawScreen World{screenType="On route"} w = onRouteScreen w w
drawScreen World{screenType="Shop"} w = shopScreen w w
drawScreen World{screenType="Settlement"} w = settlementScreen w w
drawScreen World{screenType="River"} w = riverScreen w w
drawScreen World{screenType="Inventory"} w = inventoryScreen w w
drawScreen World{screenType="Splash"} w = splashScreen w w
drawScreen World{screenType=""} w = settlementScreen w w -- for testing, remove or keep for showing error


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

-- this kinda looks bad
sinePolyGen :: Float -> Float -> Float -> Float -> Picture
sinePolyGen x y n step = Pictures (map (\s -> Translate (0) (s) (Line (zip [0,step..x] (map (\a -> (y/2)*(sin a)) [35,(35+step)..x])))) [0..n] )

partyHealthToWord :: [Int] -> String -- TODO
partyHealthToWord partyHp = "good"

-- 1 = steady, 2 = strenuous, 3 = grueling
paceToWord :: Int -> String
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

spaceToContinue :: Picture
spaceToContinue = textWriter "Press SPACE to continue" "full"

-- retrieve bitmap data for rendering on screen TODO
-- drawBitmap :: ? -> Picture
-- drawBitmap = 

-- ********************** Screen definitions **********************
-- pattern: array of elements -> picture -> combine into one large picture

-- splash screen todo (if time)
splashScreen World{userstage = 0} w = Color white ( anchorElement "bottom full text" (textWriter "bigTxt" "full"))

-- used to test output
testScreen World{userstage = 0} w = Color white ( anchorElement "bottom full text" (textWriter "bigTxt" "full"))


-- starting screen todo
startScreen World{userstage = 0} w = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- On route 

routeStatusBackground:: Picture
routeStatusBackground = Color white (lineGen xDim 275)

routeDate:: World -> Picture
routeDate w = Translate (-halfX/4 + halfTextSpanF/2) (100) (textWriterInverted ("Date: "++(dateText w)) "full")

routeWeather:: World -> Picture
routeWeather w = Translate (-halfX/4 + halfTextSpanF/2) (60) (textWriterInverted (weatherText w) "full")

routeHealth:: World -> Picture
routeHealth w = Translate (-halfX/4 + halfTextSpanF/2) (20) (textWriterInverted (healthText w) "full")

routeFood:: World -> Picture
routeFood w = Translate (-halfX/4 + halfTextSpanF/2) (-20) (textWriterInverted (foodText w) "full")

routeLandmark:: World -> Picture
routeLandmark w = Translate (-halfX/4 + halfTextSpanF/2) (-60) (textWriterInverted (landmarkText w) "full")

routeMilesTraveled:: World -> Picture
routeMilesTraveled w = Translate (-halfX/4 + halfTextSpanF/2) (-100) (textWriterInverted (milesTraveledText w) "full")

routeStatusBar :: World -> Picture
routeStatusBar w = Translate (0) (-225) (Pictures[routeStatusBackground,routeDate w, routeWeather w, routeHealth w, routeFood w,routeLandmark w, routeMilesTraveled w])

routeNearPlane :: Picture -- static for now but it could be different?
routeNearPlane = Color green (lineGen xDim 175)

-- draw a line with a sine wave for a far plane effect
routeFarPlane :: Picture
routeFarPlane = Translate (-halfX) (300) (Color green (sinePolyGen (xDim+30) 10 20 40))


routeMessage :: World -> Picture
routeMessage w = Translate ((-halfX*3)/5) (15) (textWriter (message w) "half")

routeUserInput :: World -> Picture
routeUserInput w = Translate (halfX/2) (-30) (if (userstage w) == 1 then textWriter "_" "half" else textWriter (userInput w) "half")

routeDialogueBackground :: Picture
routeDialogueBackground = Pictures [Color white (lineGen ((xDim*2)/3) 165),Color black (lineGen (((xDim*2)/3)-20) (165-20))]

-- todo have message and spot for user input
routeDialogueBox :: World -> Picture
routeDialogueBox w = if (message w) /= "" then Translate (0) (5) (Pictures[routeDialogueBackground, routeMessage w,routeUserInput w]) else blank

-- load bitmap
routeWagon :: Picture
routeWagon = Text "Todo"

-- gonna need some world props or something to do the animation of approching river
routeRiver :: World -> Picture
routeRiver w = Text "Todo"

-- message to show user input options
routePausePrompt :: Picture
routePausePrompt = Text "TODO black background, white text with saying press enter to size up the situation"

-- (stage 0 = traveling stage, stage 1 = stopped, 2 = stopped dialogue box)
onRouteScreen :: World -> World -> Picture
onRouteScreen World{userstage = 0} w = Pictures[routeNearPlane,routeStatusBar w, routeDialogueBox w,routeFarPlane]
onRouteScreen World{userstage = 1} w = Pictures[routeNearPlane,routeStatusBar w, routeFarPlane]
onRouteScreen World{userstage = 2} w = Pictures[routeNearPlane,routeStatusBar w, routeDialogueBox w,routeFarPlane]

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
rationsToWord :: Int -> String
rationsToWord ra
    | ra == 1 = "filling"
    | ra == 2 = "strenuous"
    | otherwise = "grueling"

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
settleChoice w = Translate (-100) (-325) (textWriter ("What is your choice? "++(userInput w)++"_") "half")

settleItemsList :: Picture
settleItemsList =  Translate (-175) (200) (textWriterFormatted invItemText)

oxenGui :: World -> Picture
oxenGui w = Translate (0) (0) (textWriterFormatted [(show (oxen w))])

foodGui :: World -> Picture
foodGui w = Translate (0) (-50) (textWriterFormatted [(show (food w))])

sparePartsGui :: World -> Picture
sparePartsGui w = Translate (0) (-100) (textWriterFormatted [(show (parts w))])

clothingGui :: World -> Picture
clothingGui w = Translate (0) (-150) (textWriterFormatted [(show (clothing w))])

medicineGui :: World -> Picture
medicineGui w = Translate (0) (-200) (textWriterFormatted [(show (medicine w))])

cashGui :: World -> Picture
cashGui w = Translate (-20) (-250) (textWriterFormatted ["$"++(show (cash w))]) 


settleItemValues :: World -> Picture
settleItemValues w = Translate (200) (200) (Pictures[oxenGui w,foodGui w,sparePartsGui w, clothingGui w, medicineGui w, cashGui w])

settleSuppliesHeader :: Picture
settleSuppliesHeader = anchorElement "top half text" (textWriter "Your Supplies" "half")

paceChangeCurrent :: World -> Picture
paceChangeCurrent w = Translate (-textHeightF) (yDim/2 - textHeightF) (textWriterFormatted ["Change pace","(currently: "++(paceToWord (pace w))++")"])

paceChangeDescription :: Picture
paceChangeDescription = Translate (-xDim/4) (yDim/4 - textHeightF*3) (textWriterFormatted paceChangeInfoText)

foodChangeCurrent :: World -> Picture
foodChangeCurrent w =  Translate (-textHeightF*2) (yDim/2 - textHeightF) (textWriterFormatted ["Change food rations","(currently: "++(rationsToWord (rationing w))++")"])

foodInfoDescription :: Picture
foodInfoDescription = Translate (-xDim/4) (yDim/4) (textWriter foodInfoText "half")

foodChangeDescription :: Picture
foodChangeDescription = Translate (-xDim/4) (yDim/4 - textHeightF*4) (textWriterFormatted foodChoicesText)

daysToRestInput :: World -> Picture
daysToRestInput w = Translate (-xDim/4) (20) (textWriter ("How many days would you like to rest?") "full")

blankInput :: World -> Picture
blankInput w = Translate (-xDim/16) (-40) (textWriter (userText w) "full")

daysToRestDialogue :: World -> Picture
daysToRestDialogue w = Pictures [routeDialogueBackground,daysToRestInput w, blankInput w]


-- River TODO 
riverScreen World{userstage = 0} w = Color white ( anchorElement "bottom full text" (textWriter "todo" "full"))

-- Inventory TODO put gold bars on pace,rationing
inventoryActions :: Picture
inventoryActions = Translate (-400) (75) (textWriterFormatted invActionsText)

-- overview
inventoryScreen World{userstage = 0} w = Pictures [settleDate w,settleStatusBar w, inventoryActions, settleChoice w]
-- 1 should not appear as it should switch to on route
-- supplies
inventoryScreen World{userstage = 2} w = Pictures [settleItemsList,settleSuppliesHeader, settleItemValues w, Translate (-200) (textHeightF-yDim/2) (spaceToContinue)]
--map TODO
inventoryScreen World{userstage = 3} w = Pictures [Translate (-200) (textHeightF-yDim/2) (spaceToContinue)]
-- change pace
inventoryScreen World{userstage = 4} w = Pictures [paceChangeCurrent w,settleChoice w,paceChangeDescription]
--change rations
inventoryScreen World{userstage = 5} w = Pictures [settleChoice w, foodChangeCurrent w,foodChangeDescription, foodInfoDescription]
-- stop to rest
inventoryScreen World{userstage = 6} w = Pictures [settleDate w,settleStatusBar w, inventoryActions, daysToRestDialogue w]

-- anti crash for unknown userstage
inventoryScreen World{userstage = _} w = Pictures [settleDate w,settleStatusBar w, inventoryActions, settleChoice w]

-- TODO all the other screens for actions -_-
settlementScreen World{userstage = 0} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, settleChoice w]
-- 1 should not appear as it should switch to on route
-- supplies
settlementScreen World{userstage = 2} w = Pictures [settleItemsList,settleSuppliesHeader, settleItemValues w, Translate (-200) (textHeightF-yDim/2) (spaceToContinue)]
--map TODO
settlementScreen World{userstage = 3} w = Pictures [Translate (-200) (textHeightF-yDim/2) (spaceToContinue)]
-- change pace
settlementScreen World{userstage = 4} w = Pictures [paceChangeCurrent w,settleChoice w,paceChangeDescription]
--change rations
settlementScreen World{userstage = 5} w = Pictures [settleChoice w, foodChangeCurrent w,foodChangeDescription, foodInfoDescription]
-- stop to rest
settlementScreen World{userstage = 6} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, daysToRestDialogue w]
-- 7 should not appear as it should switch to shop

-- anti crash for unknown userstage
settlementScreen World{userstage = _} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, settleChoice w]
-- settlementScreen World{userstage = 1} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, settleChoice w]


-- ******************* End of screen definitions *******************