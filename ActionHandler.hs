module ActionHandler where

import Definitions
import Helpers
import Map
import Shop
import Date


-- *************** Constants ***************

restHealthIncreaseVeryPoor = 20
restHealthIncreasePoor = 15
restHealhIncreaseFair = 10
restHealthIncreaseGood = 5



--- ***************


-- ****************************** Handler Functions ******************************

-- !! TODO: check if use **user_input :: String** as input param?
-- Set uerInput to rationing
-- rationing is one of {1, 2, 3}
setRation :: World -> World
setRation w = let r = read (userInput w) :: Int
                  in w {rationing = r}


-- !! TODO: check if take user_input :: String as input param?
-- Set userInput to pace
-- pace is one of {1, 2, 3}
setPace :: World -> World
setPace w = let p = read (userInput w) :: Int
                in w {pace = p}


-- ****** Use Medicine *******
-- Assumes userInput is in [1..5] corresponding to party member that uses the medicine
-- Cures all ailments 
useMedicine :: World -> World
useMedicine w = let temp = read (userInput w) :: Int
                    mem = temp - 1  -- to account for 0 based indexing
                    memberConditions = partyConditions w
                    cured = []
                    in w {partyConditions = replaceNth memberConditions mem cured}
-- **************************



-- ******* Update Balance, inventory after purchases *******
-- uses Shop in (currentLocation w)
updateInvBalPurchase :: World -> World
updateInvBalPurchase w = let thisShop = (shop (currentLocation w))
                             invalidReset = shopCons (store thisShop) (items thisShop) []   -- work done
                             thisNode = currentLocation w                                   -- if invalid purchase,
                             resetShop = thisNode {shop = invalidReset}                     -- resets selected field
                             purchases = selected thisShop
                             cost = getPurchaseTotal purchases
                             wallet = cash w                --
                             numFood = food w               --
                             numOxen = oxen w               --
                             numClothes = clothing w        --
                             numMeds = medicine w           -- original values
                             numParts = parts w             --
                             newWorld
                                 | cost > wallet = w {currentLocation = resetShop, message = "Not enough cash! Select fewer items."}
                                 | otherwise = w {food = numFood + f, clothing = numClothes + c, 
                                                  medicine = numMeds + m, parts = numParts + p, 
                                                  cash = wallet - cost, oxen = numOxen + o} where
                                     f = getFoodTotal purchases
                                     c = getClothingTotal purchases
                                     m = getMedicineTotal purchases
                                     p = getPartsTotal purchases
                                     o = getOxenTotal purchases
                            in newWorld
-- **************************


-- ******** Restore health with rest *********
-- assumes number of days to rest is in userInput :: String

restRestoreHealth :: World -> World
restRestoreHealth w = let days = read (userInput w) :: Int
                          members = partyHealth w
                          newHealth = restHealth days members
                          newDate = handleRestDate days (date w)
                          newWorld = w {date = newDate, partyHealth = newHealth}
                          in newWorld 

restHealth :: Int -> [Int] -> [Int]
restHealth 0 lst = lst
restHealth days lst = let first = lst!!0
                          secnd = lst!!1
                          third = lst!!2
                          fourth = lst!!3
                          fifth = lst!!4
                          newLst = (heal first):(heal secnd):(heal third):(heal fourth):[]
                          in restHealth (days - 1) newLst

heal :: Int -> Int
heal i
    | i == 0 = 0
    | i `elem` [1..25] = i + restHealthIncreaseVeryPoor
    | i `elem` [26..50] = i + restHealthIncreasePoor
    | i `elem` [51..75] = i + restHealhIncreaseFair
    | i `elem` [76..100] = i + restHealthIncreaseGood
    | otherwise = 100

handleRestDate :: Int -> Date -> Date
handleRestDate 0 d = d
handleRestDate days d = handleRestDate (days - 1) (updateDate d)

-- **************************
