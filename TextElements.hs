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
settleTrade :: String
settleTrade = "7. Attempt to trade"
-- settleTalk :: String -- not enough time for it to be worth it?
-- settleTalk = "8. Talk to people"
invFood :: String
invFood = "8. Hunt for food"
settleShop :: String
settleShop = "9. Buy supplies"

baseActionText :: [String]
baseActionText = ["You may:",settleContinue,settleCheck,settleMap,settleChangePace,settleFood,settleRest,settleTrade]

invActionsText :: [String]
invActionsText = baseActionText++[invFood]
settleActionsText :: [String]
settleActionsText = baseActionText++[settleShop]