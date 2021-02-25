module UpdateHandler where

import Definitions
import Date
import Helpers
import Map

-- ********************** Constants **********************

-- Rationing
rationingFoodDrain = [3, 2, 1] -- filling = 3 food/day, meager = 2 food/day, barebones = 1 food/day
rationingHealthDrain = [3, 2, 1, 0] -- indexed by food consumed. 0 food = 3 hp/day, 1 food = 2 hp/day, etc.

-- Pace
paceHealthDrain = [0, 1, 2] -- steady = 0 hp/day, strenuous = 1 hp/day, grueling = 2hp/day
paceDistanceGain = [20, 30, 40] -- steady = 20 miles/day, strenuous = 30 miles/day, grueling = 40 miles/day

-- Conditions
dysenteryHealthDrain = 5
choleraHealthDrain = 5
measlesHealthDrain = 5
feverHealthDrain = 5

-- Misc
littleWaterHealthDrain = 2
badWaterHealthDrain = 2
snakeBiteHealthDrain = 5

-- ********************** End of constants **********************


-- ********************** Update **********************

-- update w
-- Takes in a world and steps forward by one day, causing all the effects necessary:
--   - (Possibly) generate a random event
-- Returns the updated world
update :: World -> World
update w = let partyHealths = (partyHealth w)
               partyDeaths = (partyIsDead w)
               names = (partyNames w)
               distToNextLandmark = (distToLandmark (currentLocation w) (nextLocation w))
               newWorld
                | all (==True) partyDeaths                         = w {screenType = "Game over"}
                | (partyHealths!!0 <= 0) && (not (partyDeaths!!0)) = w {partyIsDead = replaceNth partyDeaths 0 True, message = names!!0 ++ " has died.", userstage = 2}
                | (partyHealths!!1 <= 0) && (not (partyDeaths!!1)) = w {partyIsDead = replaceNth partyDeaths 1 True, message = names!!1 ++ " has died.", userstage = 2}
                | (partyHealths!!2 <= 0) && (not (partyDeaths!!2)) = w {partyIsDead = replaceNth partyDeaths 2 True, message = names!!2 ++ " has died.", userstage = 2}
                | (partyHealths!!3 <= 0) && (not (partyDeaths!!3)) = w {partyIsDead = replaceNth partyDeaths 3 True, message = names!!3 ++ " has died.", userstage = 2}
                | (partyHealths!!4 <= 0) && (not (partyDeaths!!4)) = w {partyIsDead = replaceNth partyDeaths 4 True, message = names!!4 ++ " has died.", userstage = 2}
                | distToNextLandmark <= 0                          = let newCurr = (nextLocation w)
                                                                         newWorld
                                                                            | (isNextEmpty newCurr) = w {screenType = "Win"}
                                                                            | (hasBranch newCurr)   = w {screenType = "Settlement", userstage = 8, currentLocation = newCurr}
                                                                            | otherwise             = w {screenType = "Settlement", userstage = 0, currentLocation = newCurr, nextLocation = (getFirstInNext newCurr)}
                                                                         in newWorld
                | otherwise                                        = let newW = (randomEvent (applyPaceRationingConditions w))
                                                                         oldDate = date newW
                                                                         oldMilesTravelled = milesTravelled newW
                                                                         pacing = pace newW
                                                                         distanceGain = min (paceDistanceGain!!(pacing - 1)) distToNextLandmark
                                                                         oldDist = dist (currentLocation w)
                                                                         newCurr = (currentLocation w) {dist = oldDist + distanceGain}
                                                                         in newW {date = updateDate oldDate, milesTravelled = oldMilesTravelled + distanceGain, currentLocation = newCurr}
               in newWorld

-- ********************** End of update **********************


-- ********************** Update helpers **********************

-- applyPaceRationingConditions w
-- Takes in a world and applies all pace, rationing, and conditioning effects
-- Returns the updated world
applyPaceRationingConditions :: World -> World
applyPaceRationingConditions w = (applyPace (applyRationing (applyPartyConditions w)))

-- applyRationing n w
-- Takes in the current world, and decrements food count and party health accordingly
applyRationing :: World -> World
applyRationing w = (applyNthMemberRationing 0
                       (applyNthMemberRationing 1
                           (applyNthMemberRationing 2
                               (applyNthMemberRationing 3
                                   (applyNthMemberRationing 4 w)))))

-- applyNthMemberRationing n w
-- Takes in a party member number n and the current world, and decrements food count and member health accordingly
applyNthMemberRationing :: Int -> World -> World
applyNthMemberRationing n w = let partyHealths = partyHealth w
                                  memberHealth = partyHealths!!n
                                  rationLevel = rationing w
                                  foodHave = food w
                                  foodNeed = rationingFoodDrain!!(rationLevel - 1)
                                  foodConsumed = min foodHave foodNeed
                                  healthDrain = rationingHealthDrain!!foodConsumed
                                  newWorld
                                    | memberHealth <= 0 = w -- Party member is dead, do nothing
                                    | otherwise         = w {food = foodHave - foodConsumed, partyHealth = replaceNth partyHealths n (memberHealth - healthDrain)}
                                  in newWorld

-- applyPace w
-- Takes in the current world, and decrements party health accordingly
applyPace :: World -> World
applyPace w = (applyNthMemberPace 0
                  (applyNthMemberPace 1
                      (applyNthMemberPace 2
                          (applyNthMemberPace 3
                              (applyNthMemberPace 4 w)))))

-- applyNthMemberPace n w
-- Takes in a party member number n and the current world, and causes party member n to lose health accordingly
applyNthMemberPace :: Int -> World -> World
applyNthMemberPace n w = let partyHealths = partyHealth w
                             memberHealth = partyHealths!!n
                             pacing = pace w
                             healthDrain = paceHealthDrain!!(pacing - 1)
                             newWorld
                                | memberHealth <= 0 = w -- Party member is dead, do nothing
                                | otherwise         = w {partyHealth = replaceNth partyHealths n (memberHealth - healthDrain)}
                             in newWorld

-- applyPartyConditions w
-- Takes in a world and causes effects based on the party's conditions, if any
-- Returns the updated world
applyPartyConditions :: World -> World
applyPartyConditions w = (applyNthMemberConditions 0
                             (applyNthMemberConditions 1 
                                 (applyNthMemberConditions 2
                                     (applyNthMemberConditions 3 
                                         (applyNthMemberConditions 4 w)))))

-- applyNthMemberConditions n w
-- Takes in a number n 0 <= n <= 4 and causes effects based on party member n's conditions, if any
-- Returns the updated world
applyNthMemberConditions :: Int -> World -> World
applyNthMemberConditions n w = (applyFever n (applyMeasles n (applyCholera n (applyDysentery n w))))

-- applyDysentery n w
-- Takes in a party member number n and the current world, and causes party member n to lose health if they have dysentery
-- Returns the updated world
applyDysentery :: Int -> World -> World
applyDysentery n w = let partyHealths = partyHealth w
                         memberHealth = partyHealths!!n
                         memberConditions = (partyConditions w)!!n
                         newWorld
                            | memberHealth <= 0                   = w -- Party member is dead, do nothing
                            | "dysentery" `elem` memberConditions = w {partyHealth = replaceNth partyHealths n (memberHealth - dysenteryHealthDrain)}
                            | otherwise                           = w
                         in newWorld

-- applyCholera n memberHealth memberConditions w
-- Takes in a party member number n and the current world, and causes party member n to lose health if they have cholera
-- Returns the updated world
applyCholera :: Int -> World -> World
applyCholera n w = let partyHealths = partyHealth w
                       memberHealth = partyHealths!!n
                       memberConditions = (partyConditions w)!!n
                       newWorld
                        | memberHealth <= 0                 = w -- Party member is dead, do nothing
                        | "cholera" `elem` memberConditions = w {partyHealth = replaceNth partyHealths n (memberHealth - choleraHealthDrain)}
                        | otherwise                         = w
                       in newWorld

-- applyMeasles n memberHealth memberConditions w
-- Takes in a party member number n and the current world, and causes party member n to lose health if they have measles
-- Returns the updated world
applyMeasles :: Int -> World -> World
applyMeasles n w = let partyHealths = partyHealth w
                       memberHealth = partyHealths!!n
                       memberConditions = (partyConditions w)!!n
                       newWorld
                        | memberHealth <= 0                 = w -- Party member is dead, do nothing
                        | "measles" `elem` memberConditions = w {partyHealth = replaceNth partyHealths n (memberHealth - measlesHealthDrain)}
                        | otherwise                         = w
                       in newWorld

-- applyFever n memberHealth memberConditions w
-- Takes in a party member number n and the current world, and causes party member n to lose health if they have fever
-- Returns the updated world
applyFever :: Int -> World -> World
applyFever n w = let partyHealths = partyHealth w
                     memberHealth = partyHealths!!n
                     memberConditions = (partyConditions w)!!n
                     newWorld
                        | memberHealth <= 0               = w -- Party member is dead, do nothing
                        | "fever" `elem` memberConditions = w {partyHealth = replaceNth partyHealths n (memberHealth - feverHealthDrain)}
                        | otherwise                       = w
                     in newWorld

-- ********************** End of update helpers **********************


-- ********************** Random Event Generator **********************

randomEvent :: World -> World
randomEvent w = let (newW, n) = generateRandomInt w 100
                    newWorld
                        | n `elem` [0..27]  = noEvent newW
                        | n `elem` [28..30] = theftCash newW
                        | n `elem` [31..33] = theftFood newW
                        | n `elem` [34..36] = theftParts newW
                        | n `elem` [37..39] = theftClothing newW
                        | n `elem` [40..42] = wagonFireClothing newW
                        | n `elem` [43..45] = wagonDamage newW
                        | n `elem` [46..48] = snakeBite newW
                        | n `elem` [49..51] = wagonFireCash newW
                        | n `elem` [52..54] = findOxen newW
                        | n `elem` [55..57] = wagonFireFood newW
                        | n `elem` [58..60] = findFood newW
                        | n `elem` [61..63] = veryLittleWater newW
                        | n `elem` [64..66] = findCash newW
                        | n `elem` [67..69] = badWater newW
                        | n `elem` [70..72] = theftOxen newW
                        | n `elem` [73..75] = fever newW
                        | n `elem` [76..78] = lostTrail newW
                        | n `elem` [79..81] = wrongTrail newW
                        | n `elem` [82..84] = dysentery newW
                        | n `elem` [85..87] = cholera newW
                        | n `elem` [88..90] = killOxen newW
                        | n `elem` [91..93] = findWildFruit newW
                        | n `elem` [94..96] = measles newW
                        | n `elem` [97..99] = findWildVegetables newW
                    newUserstage
                        | (message newWorld == "") = 0
                        | otherwise = 2
                    in newWorld {userstage = newUserstage}

-- ********************** End of Random Event Generator **********************


-- ********************** Events **********************

-- Each event takes in the World state and outputs the new World state
-- Each event updates the message field to contain a description of what happened,
--   to be displayed to the player. "" if no event occurred.

-- Nothing happened
noEvent :: World -> World
noEvent w = w {message = ""}

-- Find a random number of oxen between 1 and 5 inclusive
findOxen :: World -> World
findOxen w = let (newW, n) = generateRandomInt w 5
                 numOxen = (oxen newW)
                 in newW {oxen = numOxen + (n+1), message = "You find an abandoned wagon with " ++ show (n+1) ++ " oxen."}

-- Find a random amount of food between 100 and 300 inclusive
findFood :: World -> World
findFood w = let (newW, n) = generateRandomInt w 201
                 numFood = (food newW)
                 in newW {food = numFood + (n+100), message = "You find an abandoned wagon with " ++ show (n+100) ++ " lbs of food."}

-- Find a random amount of cash between 100 and 300 inclusive
findCash :: World -> World
findCash w = let (newW, n) = generateRandomInt w 201
                 numCash = (cash newW)
                 in newW {cash = numCash + (fromIntegral (n+100)), message = "You find an abandoned wagon with $" ++ show (n+100) ++ "."}

-- Find some wild fruit
findWildFruit :: World -> World
findWildFruit w = let numFood = (food w)
                      in w {food = numFood + 20, message = "Find wild fruit."}

-- Find some wild veggies
findWildVegetables :: World -> World
findWildVegetables w = let numFood = (food w)
                           in w {food = numFood + 20, message = "Find wild vegetables."}

-- Lost trail, lose a random number of days between 1 and 3 inclusive
lostTrail :: World -> World
lostTrail w = let (newW, n) = generateRandomInt w 3
                  newWorld = lostTrailNDays (n+1) newW
                  in newWorld {message = "Lost trail. Lose " ++ show (n+1) ++ " days."}

-- Wrong trail, lose a random number of days between 1 and 3 inclusive
wrongTrail :: World -> World
wrongTrail w = let (newW, n) = generateRandomInt w 3
                   newWorld = lostTrailNDays (n+1) newW
                   in newWorld {message = "Wrong trail. Lose " ++ show (n+1) ++ " days."}

-- Lost trail for n days
lostTrailNDays :: Int -> World -> World
lostTrailNDays n w
    | n == 0 = w
    | otherwise = let newW = applyPaceRationingConditions w
                      oldDate = date newW
                      in lostTrailNDays (n-1) (newW {date = updateDate oldDate})

-- Reduces world.oxen by 1 if world.oxen > 1
killOxen :: World -> World
killOxen w
    | (oxen w) == 1 = noEvent w -- there is only 1 oxen left, do nothing
    | otherwise     = let numOxen = (oxen w)
                          in w {oxen = numOxen - 1, message = "One of the oxen has died."}

-- Reduces world.oxen by a random number between 1 and (world.oxen - 1)
theftOxen :: World -> World
theftOxen w 
    | (oxen w) == 1 = noEvent w -- there is only 1 oxen left, do nothing
    | otherwise     = let numOxen = (oxen w)
                          (newW, n) = generateRandomInt w (numOxen - 1)
                          scaledN = n + 1
                          in newW {oxen = numOxen - scaledN, message = "A thief comes during the night and steals " ++ show scaledN ++ " oxen."}

-- Reduces food by a random number
theftFood :: World -> World
theftFood w
    | (food w) == 0 = noEvent w -- there is no food to steal, do nothing
    | otherwise     = let numFood = (food w)
                          (newW, n) = generateRandomInt w numFood
                          scaledN = n + 1
                          in newW {food = numFood - scaledN, message = "A thief comes during the night and steals " ++ show scaledN ++ " lbs of food."}


-- Reduces cash by a random number
theftCash :: World -> World
theftCash w
    | (cash w) < 1.0 = noEvent w -- there is no cash to steal, do nothing
    | otherwise      = let numCash = (cash w)
                           (newW, n) = generateRandomInt w (truncate numCash)
                           scaledN = n + 1
                           in newW {cash = numCash - (fromIntegral scaledN), message = "A thief comes during the night and steals $" ++ show scaledN ++ " worth of cash."}

-- Reduces clothing by a random number
theftClothing :: World -> World
theftClothing w 
    | (clothing w) == 0 = noEvent w -- there is no clothing to steal, do nothing
    | otherwise         = let numClothing = (clothing w)
                              (newW, n) = generateRandomInt w numClothing
                              scaledN = n + 1
                              in newW {clothing = numClothing - scaledN, message = "A thief comes during the night and steals " ++ show scaledN ++ " sets of clothing."}

-- Reduces parts by a random number
theftParts :: World -> World
theftParts w 
    | (parts w) == 0 = noEvent w -- there are no parts to steal, do nothing
    | otherwise      = let numParts = (parts w)
                           (newW, n) = generateRandomInt w numParts
                           scaledN = n + 1
                           in newW {parts = numParts - scaledN, message = "A thief comes during the night and steals " ++ show scaledN ++ " spare parts."}

-- Reduces food by a random number
wagonFireFood :: World -> World
wagonFireFood w 
    | (food w) == 0 = noEvent w -- there is no food to burn, do nothing
    | otherwise     = let numFood = (food w)
                          (newW, n) = generateRandomInt w numFood
                          scaledN = n + 1
                          in newW {food = numFood - scaledN, message = "A fire in the wagon results in the loss of " ++ show scaledN ++ " lbs of food."}

-- Reduces cash by a random number
wagonFireCash :: World -> World
wagonFireCash w 
    | (cash w) < 1.0 = noEvent w -- there is no cash to burn, do nothing
    | otherwise      = let numCash = (cash w)
                           (newW, n) = generateRandomInt w (truncate numCash)
                           scaledN = n + 1
                           in newW {cash = numCash - (fromIntegral scaledN), message = "A fire in the wagon results in the loss of $" ++ show scaledN ++ " worth of cash."}

-- Reduces clothing by a random number
wagonFireClothing :: World -> World
wagonFireClothing w 
    | (clothing w) == 0 = noEvent w -- there is no clothing to burn, do nothing
    | otherwise     = let numClothing = (clothing w)
                          (newW, n) = generateRandomInt w numClothing
                          scaledN = n + 1
                          in newW {clothing = numClothing - scaledN, message = "A fire in the wagon results in the loss of " ++ show scaledN ++ " sets of clothing."}

-- Reduces spare parts by 1 if there are any. If there aren't any, lose 1-3 days
wagonDamage :: World -> World
wagonDamage w = let numParts = (parts w)
                    (newW, n) = generateRandomInt w 3
                    newWorld
                        | numParts > 0 = newW {parts = numParts - 1, message = "Your wagon broke, but thankfully you had spare parts."}
                        | otherwise    = (lostTrailNDays (n+1) w) {message = "Your wagon broke, and you have no spare parts. Lose " ++ show (n+1) ++ " days."}
                    in newWorld

-- Whole party loses health due to lack of water
veryLittleWater :: World -> World
veryLittleWater w = let partyHealthArr = (partyHealth w)
                        in w {partyHealth = map (\x -> x - littleWaterHealthDrain) partyHealthArr, message = "Very little water."}

-- Whole party loses health due to bad water
badWater :: World -> World
badWater w = let partyHealthArr = (partyHealth w)
                 in w {partyHealth = map (\x -> x - badWaterHealthDrain) partyHealthArr, message = "Bad water."}

-- A random party member loses health once
snakeBite :: World -> World
snakeBite w = let (newW, n) = generateRandomInt w 5
                  partyHealthArr = (partyHealth newW)
                  memberHealth = partyHealthArr!!n
                  memberName = (partyNames newW)!!n
                  newWorld
                    | memberHealth <= 0 = noEvent newW -- party member is dead, do nothing
                    | otherwise = newW {partyHealth = replaceNth partyHealthArr n (memberHealth - snakeBiteHealthDrain), message = memberName ++ " has a snakebite."}
                  in newWorld

-- Gives the "dysentery" condition to a random party member
dysentery :: World -> World
dysentery w = let (newW, n) = generateRandomInt w 5
                  memberHealth = (partyHealth newW)!!n
                  oldConditions = (partyConditions newW)
                  memberConditions = oldConditions!!n
                  memberName = (partyNames newW)!!n
                  numMedicine = (medicine newW)
                  newWorld 
                    | memberHealth <= 0                   = noEvent newW -- party member is dead, do nothing
                    | "dysentery" `elem` memberConditions = noEvent newW -- party member already has dysentery, do nothing
                    | numMedicine > 0                     = newW {medicine = numMedicine - 1, message = memberName ++ " got dysentery, but thankfully you had medicine."}
                    | otherwise                           = newW {partyConditions = replaceNth oldConditions n ("dysentery":memberConditions), message = memberName ++ " has dysentery."}
                  in newWorld

-- Gives the "cholera" condition to a random party member
cholera :: World -> World
cholera w = let (newW, n) = generateRandomInt w 5
                memberHealth = (partyHealth newW)!!n
                oldConditions = (partyConditions newW)
                memberConditions = oldConditions!!n
                memberName = (partyNames newW)!!n
                numMedicine = (medicine newW)
                newWorld 
                    | memberHealth <= 0                 = noEvent newW -- party member is dead, do nothing
                    | "cholera" `elem` memberConditions = noEvent newW -- party member already has cholera, do nothing
                    | numMedicine > 0                   = newW {medicine = numMedicine - 1, message = memberName ++ " got cholera, but thankfully you had medicine."}
                    | otherwise                         = newW {partyConditions = replaceNth oldConditions n ("cholera":memberConditions), message = memberName ++ " has cholera."}
                in newWorld

-- Gives the "measles" condition to a random party member
measles :: World -> World
measles w = let (newW, n) = generateRandomInt w 5
                memberHealth = (partyHealth newW)!!n
                oldConditions = (partyConditions newW)
                memberConditions = oldConditions!!n
                memberName = (partyNames newW)!!n
                numMedicine = (medicine newW)
                newWorld 
                    | memberHealth <= 0                 = noEvent newW -- party member is dead, do nothing
                    | "measles" `elem` memberConditions = noEvent newW -- party member already has measles, do nothing
                    | numMedicine > 0                   = newW {medicine = numMedicine - 1, message = memberName ++ " got measles, but thankfully you had medicine."}
                    | otherwise                         = newW {partyConditions = replaceNth oldConditions n ("measles":memberConditions), message = memberName ++ " has measles."}
                in newWorld

-- Gives the "fever" condition to a random party member
fever :: World -> World
fever w = let (newW, n) = generateRandomInt w 5
              memberHealth = (partyHealth newW)!!n
              oldConditions = (partyConditions newW)
              memberConditions = oldConditions!!n
              memberName = (partyNames newW)!!n
              numMedicine = (medicine newW)
              newWorld 
                | memberHealth <= 0               = noEvent newW -- party member is dead, do nothing
                | "fever" `elem` memberConditions = noEvent newW -- party member already has fever, do nothing
                | numMedicine > 0                 = newW {medicine = numMedicine - 1, message = memberName ++ " got a fever, but thankfully you had medicine."}
                | otherwise                       = newW {partyConditions = replaceNth oldConditions n ("fever":memberConditions), message = memberName ++ " has a fever."}
              in newWorld

-- ********************** End of events **********************