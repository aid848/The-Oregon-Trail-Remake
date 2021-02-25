module ScreenHandler where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Definitions
import TextElements
import Date
import Shop
import Map

textColor = white

-- top level screen drawer based on world state TODO refactor
drawScreen :: World -> World -> Picture
drawScreen World{screenType="Start"} w = startScreen w w -- done
drawScreen World{screenType="On route"} w = onRouteScreen w w -- mostly done except for wagon graphics, and distances, partyHealthToWord, paceToWord
drawScreen World{screenType="Shop"} w = shopScreen w w -- done, except fix money to have x.xx, set userinput to _
drawScreen World{screenType="Settlement"} w = settlementScreen w w -- done, except map
drawScreen World{screenType="River"} w = riverScreen w w -- done, except depth and width
drawScreen World{screenType="Inventory"} w = inventoryScreen w w -- done, except map
drawScreen World{screenType="Splash"} w = splashScreen w w -- not started, prob canceled
drawScreen World{screenType="Game over"} w = gameOver w w -- done (kinda lame)
drawScreen World{screenType="Win"} w = win w w -- done (kinda lame)
drawScreen World{screenType=_} w = blank


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

averageInt :: [Int] -> Int -> Int
averageInt lst len = (foldr (+) 0 lst) `div` len

partyHealthToWord :: [Int] -> String
partyHealthToWord partyHp
    | (averageInt partyHp 5) > 75 = "good"
    | (averageInt partyHp 5) > 50 = "fair"
    | (averageInt partyHp 5) > 25 = "poor"
    | otherwise = "very poor"

paceToWord :: Int -> String
paceToWord val
    |val == 1 = "steady"
    |val == 2 = "strenuous"
    |otherwise = "grueling"

dateText :: World -> String
dateText w = (month (date w))++" "++(show (day (date w)))++", "++(show (year (date w)))

weatherText :: World -> String
weatherText w = ("Weather: "++(weather w))

healthText :: World -> String
healthText w = ("Health: "++(partyHealthToWord(partyHealth w)))

foodText :: World -> String
foodText w = ("Food: "++(show (food w))++" pounds")


settleChoice :: World -> Picture
settleChoice w = Translate (-100) (-325) (textWriter ("What is your choice? "++(userInput w)) "half")

choiceGeneric :: String -> World -> Picture
choiceGeneric q w = (textWriter (q++(userInput w)) "half")

--todo change to remaining distance
landmarkText :: World -> String
landmarkText w = ("Next landmark: "++(show (distToLandmark (currentLocation w) (nextLocation w)))++" miles")
-- landmarkText w = ("Next landmark: "++"TODO"++" miles")

milesTraveledText :: World -> String
milesTraveledText w = ("Miles Traveled: "++(show (milesTravelled w))++" miles")

spaceToContinue :: Picture
spaceToContinue = textWriter "Press SPACE to continue" "full"

enterToContinue :: Picture
enterToContinue = textWriter "Enter name and press ENTER to continue" "full"

-- retrieve bitmap data for rendering on screen TODO
-- drawBitmap :: ? -> Picture
-- drawBitmap = 

-- ********************** Screen definitions **********************
-- pattern: array of elements -> picture -> combine into one large picture

-- splash screen todo (if time)
splashScreen World{userstage = 0} w = Color white ( anchorElement "bottom full text" (textWriter "bigTxt" "full"))

introNumberedNames :: Picture
introNumberedNames = Translate (-xDim/4) (0) (textWriterFormatted oneToFive)

introNames :: Int -> World -> Picture
introNames i w = Translate (-xDim/6) (0) (textWriterFormatted (take i (partyNames w)))

titleHeader :: Picture
titleHeader = Translate (-xDim/6) (yDim/2 - textHeightF*2) (Color yellow (arrangeText (foldr (\ x y -> (Scale 0.5 0.5(Text x)):y) [] (splitText "The Oregon Trail" halfTextSpan))))

introWealthText :: Picture
introWealthText = Translate (-xDim/6) (yDim/2 - textHeightF*4) (textWriter introWealth "half")

introWealthOptions:: Picture
introWealthOptions = Translate (-xDim/6) (yDim/2 - textHeightF*8) (textWriterFormatted introWealthOptionsText)

introMonths :: Picture
introMonths = Translate (-xDim/8) (yDim/8) (textWriterFormatted introLocationMonths)

introLocationInfoText :: Picture
introLocationInfoText = anchorElement "top full text" (textWriter introLocationInfo "full")

introOutro :: World -> Picture
introOutro  w = Translate (halfTextSpanF - halfX) (0) (textWriter (introOutTextOne++(show (cash w))++introOutTextTwo) "full")

outroWarning :: Picture
outroWarning = Translate (halfTextSpanF - halfX) (-150) (textWriter introShopWarning "full")

-- starting screen
-- choose background (wealth)
startScreen World{userstage = 0} w = Pictures [titleHeader, introWealthOptions, settleChoice w, introWealthText]
-- party leader name 
startScreen World{userstage = 1} w = Pictures [titleHeader,Translate (-xDim/4) (0) (choiceGeneric introWagonLeader w),Translate (-xDim/4) (-yDim/2 + textHeightF) enterToContinue]
-- party member 2 name 
startScreen World{userstage = 2} w = Pictures [(introNames 1 w),introNumberedNames,titleHeader,Translate (-xDim/6) (-50) (textWriter (userText w) "half"),Translate (-xDim/4) (-yDim/2 + textHeightF) enterToContinue]
-- party member 3 name 
startScreen World{userstage = 3} w = Pictures [(introNames 2 w),introNumberedNames,titleHeader,Translate (-xDim/6) (-100) (textWriter (userText w) "half"),Translate (-xDim/4) (-yDim/2 + textHeightF) enterToContinue]
-- party member 4 name 
startScreen World{userstage = 4} w = Pictures [(introNames 3 w),introNumberedNames,titleHeader,Translate (-xDim/6) (-150) (textWriter (userText w) "half"),Translate (-xDim/4) (-yDim/2 + textHeightF) enterToContinue]
-- party member 5 name
startScreen World{userstage = 5} w = Pictures [(introNames 4 w),introNumberedNames,titleHeader,Translate (-xDim/6) (-200) (textWriter (userText w) "half"),Translate (-xDim/4) (-yDim/2 + textHeightF) enterToContinue]
-- starting month select (march-july 1948) 
startScreen World{userstage = 6} w = Pictures[Translate (-xDim/8) (0) (settleChoice w),introMonths, introLocationInfoText ]
-- info text 
startScreen World{userstage = 7} w = Pictures [outroWarning,titleHeader,introOutro w,(Translate (-200) (textHeightF-yDim/2) spaceToContinue)]
-- anti crash 
startScreen World{userstage = _} w = Pictures [titleHeader, introWealthOptions, settleChoice w, introWealthText]


-- On route 

routeStatusBackground:: Picture
routeStatusBackground = Translate (0) (15) (Color white (lineGen xDim 255))

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

-- message and spot for user input
routeDialogueBox :: World -> Picture
routeDialogueBox w = if (message w) /= "" then Translate (0) (5) (Pictures[routeDialogueBackground, routeMessage w,routeUserInput w]) else blank

-- load bitmap

routeWagon :: World -> Picture
routeWagon w
    | (milesTravelled w) `mod` 2 == 1 = routeWagonOne w
    | otherwise = routeWagonTwo w

routeWagonOne :: World -> Picture
routeWagonOne w = Translate (xDim/4) ((yDim*2)/7) ((imgs w)!!1)

routeWagonTwo :: World -> Picture
routeWagonTwo w = Translate (xDim/4) (215) (Scale 0.42 0.4 ((imgs w)!!3))
-- on route screen

-- message to show user input options
routePausePrompt :: Picture
routePausePrompt = Translate (-xDim/4) (-yDim/2+10) (textWriter "Press ENTER to size up the situation" "full")

onRouteScreen :: World -> World -> Picture
-- traveling stage, space to size up the situation
onRouteScreen World{userstage = 0} w = Pictures [Translate (0) (30) (Pictures[routeNearPlane,routeStatusBar w,routeFarPlane]), routePausePrompt, routeWagon w]
-- stopped, space to continue
onRouteScreen World{userstage = 1} w = Pictures [Translate (0) (30) (Pictures[routeNearPlane,routeStatusBar w, routeFarPlane]), Translate (-xDim/6) (-yDim/2+10) spaceToContinue, routeWagon w]
-- stopped dialogue box space to continue
onRouteScreen World{userstage = 2} w = Pictures [Translate (0) (30) (Pictures[routeNearPlane,routeStatusBar w, routeDialogueBox w,routeFarPlane]), Translate (-xDim/6) (-yDim/2+10) spaceToContinue, routeWagon w]
-- anti crash
onRouteScreen World{userstage = _} w = Pictures [Translate (0) (30) (Pictures[routeNearPlane,routeStatusBar w,routeFarPlane]), routePausePrompt, routeWagon w]

-- Shop

shopMoneyCount :: World -> Picture
shopMoneyCount w = Translate (-200) (-140) (textWriter ("Amount you have: $"++(show (cash w))) "half")

showItemSelect :: World -> Picture
showItemSelect w = Translate (-200) (-205) (textWriter ("Which item would you like to buy? "++(userInput w)) "full")

itemBuyMsg :: Picture
itemBuyMsg = Translate (-200) (-205) (textWriter "Press ENTER to confirm" "full")

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

shopItemBars :: Picture
shopItemBars = color red (Pictures[Translate (100) (350) (lineGen 700 10),Translate (100) (235) (lineGen 700 10) ])

shopMan :: World -> Picture
shopMan w = Translate (-xDim/4 - 40) (0) (Scale (0.75) (0.75) ((imgs w)!!0))

shopStaticTextElements :: Picture
shopStaticTextElements = Pictures [shopLeaveMessage, shopBars]

shopDynamicElements :: World -> Picture
shopDynamicElements w = Pictures [shopMoneyCount w,showItemSelect w, shopBill w, shopDate w, shopStockShow w, shopNameMessage w, shopMan w,shopDialogueBox w]

shopItemDesc :: String -> Picture
shopItemDesc txt = textWriter txt "half"

shopUnitDialog :: World -> Picture
shopUnitDialog  w = Translate (-xDim/4) (textHeightF-(yDim/4)) (textWriter ("How many units will you buy? "++(userInput w)) "full")

shopItemInfoText :: String -> Picture
shopItemInfoText txt = Translate (halfTextSpanF/2 - halfX) (0) (textWriter txt "full")

shopDialogueBox :: World -> Picture
shopDialogueBox w = if (message w) /= "" then Translate (0) (5) (Pictures[routeDialogueBackground, routeMessage w,Translate (-xDim/4) (-50) (spaceToContinue)]) else blank

shopScreen :: World -> World -> Picture
-- overview
shopScreen World{userstage = 0} w = Pictures [shopStaticTextElements,(shopDynamicElements w)]
-- oxen purchase 
shopScreen World{userstage = 1} w = Pictures [itemBuyMsg,shopItemBars,shopDate w,shopNameMessage w, Translate (-xDim/6) (-(yDim*2)/7) (shopBill w), shopUnitDialog w, shopItemInfoText oxenInfo ]
-- Food purchase 
shopScreen World{userstage = 2} w = Pictures [itemBuyMsg,shopItemBars,shopDate w,shopNameMessage w, Translate (-xDim/6) (-(yDim*2)/7) (shopBill w), shopUnitDialog w, shopItemInfoText foodInfo ]
-- Spare parts purchase 
shopScreen World{userstage = 3} w = Pictures [itemBuyMsg,shopItemBars,shopDate w,shopNameMessage w, Translate (-xDim/6) (-(yDim*2)/7) (shopBill w), shopUnitDialog w, shopItemInfoText sparePartsInfo ]
-- Clothing purchase 
shopScreen World{userstage = 4} w = Pictures [itemBuyMsg,shopItemBars,shopDate w,shopNameMessage w, Translate (-xDim/6) (-(yDim*2)/7) (shopBill w), shopUnitDialog w, shopItemInfoText clothingInfo ]
-- Medicine purchase
shopScreen World{userstage = 5} w = Pictures [itemBuyMsg,shopItemBars,shopDate w,shopNameMessage w, Translate (-xDim/6) (-(yDim*2)/7) (shopBill w), shopUnitDialog w, shopItemInfoText medicineInfo ]
-- anti crash
shopScreen World{userstage = _} w = Pictures [shopStaticTextElements,(shopDynamicElements w)]

-- Settlement (user state based on selection number)

rationsToWord :: Int -> String
rationsToWord ra
    | ra == 1 = "filling"
    | ra == 2 = "meager"
    | otherwise = "bare bones"

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
blankInput w = Translate (-xDim/16) (-40) (textWriter (userInput w) "full")

daysToRestDialogue :: World -> Picture
daysToRestDialogue w = Pictures [routeDialogueBackground,daysToRestInput w, blankInput w]

branchDialogueInput :: World -> Picture
branchDialogueInput w = Translate (-xDim/4) (40) (textWriterFormatted (["Which way will you go?"]++(getBranchNames (currentLocation w))))

branchDialogue :: World -> Picture
branchDialogue w = Pictures [routeDialogueBackground,branchDialogueInput w, blankInput w]

-- River

riverWidthText :: World -> Picture -- TODO
riverWidthText w = Translate (-xDim/6) (150) (textWriter ("River width: "++(show (width (currentLocation w)))++" feet") "Full")
riverDepthText :: World -> Picture -- TODO
riverDepthText w = Translate (-xDim/6) (100) (textWriter ("River depth: "++(show (depth (currentLocation w)))++" feet") "Full")
riverOptionsText :: Picture
riverOptionsText = Translate (halfTextSpanF - halfX/2) (0) (textWriterFormatted riverOptions)

riverGenericMessage :: String -> Picture
riverGenericMessage txt = anchorElement "bottom full text" (textWriter txt "full")

-- todo width and depth
riverSituationText :: World -> String
riverSituationText w = riverInfoOne++(show (width (currentLocation w)))++" feet across, and "++(show (depth (currentLocation w)))++" feet deep in the middle."

riverYNmsg :: Picture
riverYNmsg = Translate (320) (-325) (textWriter "(y/n)" "half")

-- info
riverScreen World{userstage = 0} w = Pictures [(riverGenericMessage (riverSituationText w)),settleName w,settleDate w,  Translate (-200) (textHeightF-yDim/2) spaceToContinue]
-- options
riverScreen World{userstage = 1} w = Pictures[settleDate w , settleName w,Translate (-xDim/8 + textHeightF/2) ((yDim*2)/7) (textWriter (weatherText w) "half"), riverOptionsText, settleChoice w, riverWidthText w, riverDepthText w]
-- ford river
riverScreen World{userstage = 2} w = Pictures[settleDate w , settleName w,(riverGenericMessage (riverFord)),Translate (-200) (textHeightF-yDim/2) spaceToContinue]
-- float across
riverScreen World{userstage = 3} w = Pictures[settleDate w , settleName w,(riverGenericMessage (riverFloatInfo)),Translate (-200) (textHeightF-yDim/2) spaceToContinue]
-- ferry across
riverScreen World{userstage = 4} w = Pictures[settleDate w , settleName w,(riverGenericMessage (riverFerryInfo)),(settleChoice w), riverYNmsg]
-- anti crash
riverScreen World{userstage = _} w = Pictures [(riverGenericMessage (riverSituationText w)),settleName w,settleDate w,  Translate (-200) (textHeightF-yDim/2) spaceToContinue]

-- Inventory TODO put gold bars on pace,rationing
inventoryActions :: Picture
inventoryActions = Translate (-400) (75) (textWriterFormatted invActionsText)

mapFullScreen :: World -> Picture
mapFullScreen w = Translate (0) (5) (Scale (0.76) (0.76) ((imgs w)!!2))

restEnterToContinue :: Picture
restEnterToContinue = Translate (halfTextSpanF*2 - halfX/2) (-150) (textWriter "Press ENTER to continue" "full")

-- overview
inventoryScreen World{userstage = 0} w = Pictures [settleDate w,settleStatusBar w, inventoryActions, settleChoice w]
-- 1 should not appear as it should switch to on route
-- supplies
inventoryScreen World{userstage = 2} w = Pictures [settleItemsList,settleSuppliesHeader, settleItemValues w, Translate (-200) (textHeightF-yDim/2) (spaceToContinue)]
--map
inventoryScreen World{userstage = 3} w = mapFullScreen w
-- change pace
inventoryScreen World{userstage = 4} w = Pictures [paceChangeCurrent w,settleChoice w,paceChangeDescription]
--change rations
inventoryScreen World{userstage = 5} w = Pictures [settleChoice w, foodChangeCurrent w,foodChangeDescription, foodInfoDescription]
-- stop to rest
inventoryScreen World{userstage = 6} w = Pictures [settleDate w,settleStatusBar w, daysToRestDialogue w, restEnterToContinue]

-- anti crash for unknown userstage
inventoryScreen World{userstage = _} w = Pictures [settleDate w,settleStatusBar w, inventoryActions, settleChoice w]

-- overview
settlementScreen World{userstage = 0} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, settleChoice w]
-- 1 should not appear as it should switch to on route
-- supplies
settlementScreen World{userstage = 2} w = Pictures [settleItemsList,settleSuppliesHeader, settleItemValues w, Translate (-200) (textHeightF-yDim/2) (spaceToContinue)]
--map
settlementScreen World{userstage = 3} w = mapFullScreen w
-- change pace
settlementScreen World{userstage = 4} w = Pictures [paceChangeCurrent w,settleChoice w,paceChangeDescription]
--change rations
settlementScreen World{userstage = 5} w = Pictures [settleChoice w, foodChangeCurrent w,foodChangeDescription, foodInfoDescription]
-- stop to rest
settlementScreen World{userstage = 6} w = Pictures [settleName w, settleDate w,settleStatusBar w, daysToRestDialogue w, restEnterToContinue]
-- 7 should not appear as it should switch to shop
-- branch select
settlementScreen World{userstage = 8} w = Pictures [settleName w, settleDate w,settleStatusBar w, settleActions, branchDialogue w]
-- anti crash for unknown userstage
settlementScreen World{userstage = _} w = Pictures [settleItemsList,settleSuppliesHeader, settleItemValues w, Translate (-200) (textHeightF-yDim/2) (spaceToContinue)]


gameOverText :: Picture
gameOverText = Translate (-xDim/3) (0) (Scale 0.3 0.3 (Color red (Text "Game over, you didn't make it to Oregon...")))

gameOver World{userstage = _} w = Pictures[gameOverText, Translate (0) (-yDim/2 + textHeightF) spaceToContinue]


winText :: Picture
winText = Translate (-xDim/3) (0) (Scale 0.3 0.3 (Color yellow (Text "Congratulations you made it to Oregon!")))

win World{userstage = _} w = Pictures[winText, Translate (0) (-yDim/2 + textHeightF) spaceToContinue]

-- ******************* End of screen definitions *******************