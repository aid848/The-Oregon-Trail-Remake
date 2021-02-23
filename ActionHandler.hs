module ActionHandler where

import Definitions
import Helpers
import Map
import Shop
import Date


-- *************** Constants ***************

wealthOptions = [1600.0, 800.0, 400.0]

dateSelect = ["March", "April", "May", "June", "July"]

restHealthIncreaseVeryPoor = 20
restHealthIncreasePoor = 15
restHealthIncreaseFair = 10
restHealthIncreaseGood = 5

-- *****************************************

--      Screen handlers
-- Start        - Done
-- On route     - Done
-- Shop         - Almost done, need to fix purchasing
-- Settlement   - Done
-- River        - Done except for "y" ferry option
-- Inventory    - Done
-- Game over    - Done


-- *********** ScreenType - based handler functions for use in KeyHandler.hs ***********


handleStartNumbers :: Int -> World -> World
handleStartNumbers num w = let currStage = userstage w
                               dateSel = currStage == 6  -- True if input chooses date
                               newWorld
                                   | dateSel = w {date = startDate, userstage = 7}
                                   | currStage == 0 = w {cash = wealth, userstage = 1}
                                   | otherwise = w
                               in newWorld where
                                   month
                                       | num >= 1 && num <= 5 = dateSelect!!(num - 1)
                                       | otherwise = "Error"
                                   startDate = dateCons 1 month 1848
                                   wealth
                                       | num >=1 && num <= 3 = wealthOptions!!(num - 1)
                                       | otherwise = 0


handleStartEnter :: World -> World
handleStartEnter w = let stage = userstage w
                         newStage = stage + 1
                         newWorld 
                             | newStage <= 6 = w {userstage = newStage}
                             | otherwise = w 
                        in newWorld

handleStartSpace :: World -> World
handleStartSpace w = let stage = userstage w
                         newWorld
                             | stage == 7 = w {screenType = "Settlement", userstage = 0}
                             | otherwise = w
                         in newWorld

handleOnRouteSpace :: World -> World
handleOnRouteSpace w = let stage = userstage w
                           newWorld
                               | stage == 0 = w {screenType = "Inventory", userstage = 0}
                               | stage == 1 = w {userstage = 0}
                               | stage == 2 = w {userstage = 0}
                               | otherwise = w
                            in newWorld

handleOnRouteCtl :: World -> World
handleOnRouteCtl w = let stage = userstage w
                         newWorld
                             | stage == 0 = w {userstage = 1}
                             | otherwise  = w
                             in newWorld

handleShopNumbers :: Int -> World -> World
handleShopNumbers num w = let stage = userstage w
                              chooseItem = num >= 1 && num <=5  -- bool to check if we're selecting item
                              newWorld
                                  | stage == 0 && chooseItem = w {userstage = num}
                                  | chooseItem = updateCart num w
                                  in newWorld

handleShopSpace :: World -> World
handleShopSpace w = let stage = userstage w
                        newWorld
                            | stage == 0 = updateInvBalPurchase w
                            | otherwise = w
                            in newWorld

handleShopEnter :: World -> World
handleShopEnter w = let stage = userstage w
                        isCartItem = stage >= 1 && stage <= 5  -- bool to check if adding item to cart
                        newWorld
                            | isCartItem = w {userstage = 0}
                            | otherwise = w
                            in newWorld
                        
handleInvNumbers :: Int -> World -> World
handleInvNumbers num w = let stage = userstage w
                             overview = stage == 0
                             pace = stage == 4
                             ration = stage == 5
                             rest = stage == 6
                             validNum = num >= 1 && num <= 6
                             validPaceRation = num >= 1 && num <= 3
                             newWorld
                                 | overview && num == 1 = w {screenType = "On route", userstage = 0}
                                 | overview && validNum = w {userstage = num}
                                 | pace && validPaceRation = w {pace = num, userstage = 0} 
                                 | ration && validPaceRation = w {rationing = num, userstage = 0}
                                 | rest  = (restRestoreHealth num w) {userstage = 0}
                                 | otherwise = w
                                 in newWorld

handleInvSpace :: World -> World 
handleInvSpace w = let stage = userstage w
                       check = stage == 2 || stage == 3
                       newWorld
                           | check = w {userstage = 0}
                           | otherwise = w
                           in newWorld


handleSettleNumbers :: Int -> World -> World 
handleSettleNumbers num w = let stage = userstage w
                                overview = stage == 0
                                pace = stage == 4
                                ration = stage == 5
                                rest = stage == 6
                                validNum = num >= 1 && num <= 7
                                validPaceRation = num >= 1 && num <= 3
                                selectBranch = overview && hasBranch (currentLocation w) && num == 8  -- bool to check if branch option
                                noBranch = getFirstInNext (currentLocation w)
                                newWorld
                                    | overview && num == 1 && not (hasBranch (currentLocation w)) = w {nextLocation = noBranch, screenType = "On route", userstage = 0}
                                    | overview && num == 1 = w {screenType = "On route", userstage = 0}
                                    | overview && num == 7 = w {screenType = "Shop", userstage = 0}
                                    | selectBranch = w {userstage = 8}
                                    | stage == 8 = w {nextLocation = nextLoc}
                                    | overview && validNum = w {userstage = num}
                                    | pace && validPaceRation = w {pace = num, userstage = 0} 
                                    | ration && validPaceRation = w {rationing = num, userstage = 0}
                                    | rest  = (restRestoreHealth num w) {userstage = 0}
                                    | otherwise = w
                                    in newWorld where
                                        nextLoc
                                            | num == 1 = getFirstInNext (currentLocation w)
                                            | num == 2 = getSecondInNext (currentLocation w)

handleSettleSpace :: World -> World 
handleSettleSpace w = let stage = userstage w
                          check = stage == 2 || stage == 3
                          newWorld
                              | check = w {userstage = 0}
                              | otherwise = w
                              in newWorld

handleRiverNumbers :: Int -> World -> World 
handleRiverNumbers num w = let stage = userstage w
                               ifOptions = stage == 1
                               validNum = num >= 1 && num <= 3
                               newWorld
                                   | ifOptions = w {userstage = num+1}
                                   | otherwise = w
                                   in newWorld

handleRiverSpace :: World -> World 
handleRiverSpace w = let stage = userstage w
                         overview = stage == 0
                         riverOptions = stage == 2 || stage == 3
                         newWorld
                             | overview = w {userstage = 1}
                             | riverOptions = w {screenType = "On route", userstage = 0}
                             | otherwise = w
                             in newWorld

handleRiverChar :: Char -> World -> World 
handleRiverChar char w = let stage = userstage w
                             ferry = stage == 4
                             yes = char == 'y'
                             newWorld
                                 | ferry && yes = w {screenType = "On route", userstage = 0} -- TODO!!! update cash and date?
                                 | ferry && not yes = w {userstage = 1}
                                 | otherwise = w
                                 in newWorld

handleGameOverSpace :: World -> World
handleGameOverSpace w = let newWorld = (nWorld w)
                            in newWorld

-- ****************************** Small Handler Functions ****************************
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





-- ********* Update Cart fields with key input ************

-- uses updateHelper in Shop.hs
-- 
--
-- checks number key pressed (amt), checks user stage for context->item
-- and shop in currentLocation for price 
updateCart :: Int -> World -> World
updateCart amt w = let oldCart = cart w
                       stage = userstage w
                       itemStr
                           | stage == 1 = "Oxen"
                           | stage == 2 = "Food"
                           | stage == 3 = "Spare Parts"
                           | stage == 4 = "Clothing"
                           | stage == 5 = "Medicine"
                       storeItems = items (shop (currentLocation w))
                       price = getItemPrice itemStr storeItems
                       cost = price * fromIntegral(amt)
                       stock = stringItUp oldCart
                       newCart
                           | oldCart == [] = [(itemStr, amt, cost)]
                           | itemStr `elem` stock = updateHelper oldCart itemStr amt cost
                           | otherwise = (itemStr, amt, cost) : oldCart
                       newWorld = w {bill = cost, cart = newCart}
                       in newWorld


-- **********************************************************





-- ******* Update Balance, inventory after purchases *******
-- uses Shop in (currentLocation w)
updateInvBalPurchase :: World -> World
updateInvBalPurchase w = let purchases = cart w 
                             cost = getPurchaseTotal purchases
                             wallet = cash w                --
                             numFood = food w               --
                             numOxen = oxen w               --
                             numClothes = clothing w        --
                             numMeds = medicine w           -- original values
                             numParts = parts w             --
                             newWorld
                                 | cost > wallet = w {screenType = "Shop", userstage = 0, message = "Not enough cash! Try again and select fewer items.", cart = []}
                                 | otherwise = w {food = numFood + f, clothing = numClothes + c, 
                                                  medicine = numMeds + m, parts = numParts + p, 
                                                  cash = wallet - cost, oxen = numOxen + o,
                                                  screenType = "Inventory", userstage = 0} where
                                     f = getFoodTotal purchases
                                     c = getClothingTotal purchases
                                     m = getMedicineTotal purchases
                                     p = getPartsTotal purchases
                                     o = getOxenTotal purchases
                            in newWorld
-- **************************


-- ******** Restore health with rest *********
-- assumes number of days to rest is in userInput :: String

restRestoreHealth :: Int -> World -> World
restRestoreHealth days w = let members = partyHealth w
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
                          newLst = (heal first):(heal secnd):(heal third):(heal fourth):(heal fifth):[]
                          in restHealth (days - 1) newLst

heal :: Int -> Int
heal i
    | i == 0 = 0
    | i `elem` [1..25] = i + restHealthIncreaseVeryPoor
    | i `elem` [26..50] = i + restHealthIncreasePoor
    | i `elem` [51..75] = i + restHealthIncreaseFair
    | i `elem` [76..100] = i + restHealthIncreaseGood
    | otherwise = 100

handleRestDate :: Int -> Date -> Date
handleRestDate 0 d = d
handleRestDate days d = handleRestDate (days - 1) (updateDate d)

-- **************************

