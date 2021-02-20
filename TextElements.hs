module TextElements where

-- store gui text

generalStoreHeader :: String
generalStoreHeader = "Matt's General Store"

storeLeave :: String
storeLeave = "Press Space Bar to leave store"


-- settlement actions
settleContinue :: String
settleContinue = "1. Continue on the trail"
settleCheck :: String
settleCheck = "2. Check supplies"
settleMap:: String
settleMap = "3. Look at map"
settleChangePace :: String
settleChangePace = "4. Change pace"
settleFood :: String
settleFood = "5. Change food rations"
settleRest :: String
settleRest = "6. Stop to rest"
-- settleTrade :: String
-- settleTrade = "7. Attempt to trade"
-- settleTalk :: String -- not enough time for it to be worth it?
-- settleTalk = "8. Talk to people"
-- invFood :: String
-- invFood = "8. Hunt for food"
settleShop :: String
settleShop = "7. Buy supplies"

baseActionText :: [String]
baseActionText = ["You may:",settleContinue,settleCheck,settleMap,settleChangePace,settleFood,settleRest]

invActionsText :: [String]
invActionsText = baseActionText
settleActionsText :: [String]
settleActionsText = baseActionText++[settleShop]


-- inventory items
invItemText :: [String]
invItemText = ["Oxen","Pounds of food","Spare parts","Sets of clothing", "Doses of Medicine","Money Left"]

paceChangeInfoText :: [String]
paceChangeInfoText = ["The pace at which you travel can change.", "        Your choices are:       ", "        1. a steady pace      ", "        2. a strenuous pace      ", "        3. a grueling pace      "]


foodInfoText :: String
foodInfoText = "The amount of food the people in   your party eat each day can change.These amounts are:"

foodFillingText :: String
foodFillingText = "1. filling - meals are large and generous."

foodMeagerText :: String
foodMeagerText = "2. meager - meals are small, but adequate."

foodBareText :: String
foodBareText = "3. bare bones - meals are very small;"

foodChoicesText :: [String]
foodChoicesText = [foodFillingText, foodMeagerText,foodBareText,"            everyone stays hungry."]