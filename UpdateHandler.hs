module UpdateHandler where

import Definitions
import Date
import Helpers

-- ********************** Constants **********************

-- Rationing
rationingFoodDrain = [3, 2, 1] -- filling = 3 food/day, meager = 2 food/day, barebones = 1 food/day
rationingHealthDrain = [3, 2, 1, 0] -- indexed by food consumed. 0 food = 3 hp/day, 1 food = 2 hp/day, etc.

-- Pace
paceHealthDrain = [0, 1, 2] -- steady = 0 hp/day, strenuous = 1 hp/day, grueling = 2hp/day

-- Conditions
dysenteryHealthDrain = 5
choleraHealthDrain = 5
measlesHealthDrain = 5

-- ********************** End of constants **********************


-- ********************** Update **********************

-- update w
-- Takes in a world and steps forward by one day, causing all the effects necessary:
--   - (Possibly) generate a random event
-- Returns the updated world
update :: World -> World
update w = let newW = (randomEvent (applyPaceRationingConditions w))
               oldDate = date newW
               in newW {date = updateDate oldDate}

-- applyPaceRationingConditions w
-- Takes in a world and applies all pace, rationing, and conditioning effects
-- Returns the updated world
applyPaceRationingConditions :: World -> World
applyPaceRationingConditions w = (applyPace (applyRationing (applyPartyConditions w)))

-- ********************** End of update **********************


-- ********************** Update helpers **********************

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
applyNthMemberConditions n w = (applyCholera n (applyDysentery n w))

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

-- ********************** End of update helpers **********************


-- ********************** Random Event Generator **********************

randomEvent :: World -> World
randomEvent w = let (newW, n) = generateRandomInt w 100
                    newWorld
                        | n `elem` [0..19]  = noEvent newW
                        | n `elem` [20..29] = findOxen newW
                        | n `elem` [30..39] = findFood newW
                        | n `elem` [40..49] = findCash newW
                        | n `elem` [50..59] = theftOxen newW
                        | n `elem` [60..64] = lostTrail newW
                        | n `elem` [64..69] = wrongTrail newW
                        | n `elem` [70..74] = dysentery newW
                        | n `elem` [75..79] = cholera newW
                        | n `elem` [80..89] = killOxen newW
                        | n `elem` [90..99] = measles newW
                    in newWorld

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

-- Gives the "dysentery" condition to a random party member
dysentery :: World -> World
dysentery w = let (newW, n) = generateRandomInt w 5
                  memberHealth = (partyHealth newW)!!n
                  oldConditions = (partyConditions newW)
                  memberConditions = oldConditions!!n
                  memberName = (partyNames newW)!!n
                  newWorld 
                    | memberHealth <= 0                   = noEvent newW -- party member is dead, do nothing
                    | "dysentery" `elem` memberConditions = noEvent newW -- party member already has dysentery, do nothing
                    | otherwise                                = newW {partyConditions = replaceNth oldConditions n ("dysentery":memberConditions), message = memberName ++ " has dysentery."}
                  in newWorld

-- Gives the "cholera" condition to a random party member
cholera :: World -> World
cholera w = let (newW, n) = generateRandomInt w 5
                memberHealth = (partyHealth newW)!!n
                oldConditions = (partyConditions newW)
                memberConditions = oldConditions!!n
                memberName = (partyNames newW)!!n
                newWorld 
                    | memberHealth <= 0                 = noEvent newW -- party member is dead, do nothing
                    | "cholera" `elem` memberConditions = noEvent newW -- party member already has cholera, do nothing
                    | otherwise                              = newW {partyConditions = replaceNth oldConditions n ("cholera":memberConditions), message = memberName ++ " has cholera."}
                in newWorld

-- Gives the "measles" condition to a random party member
measles :: World -> World
measles w = let (newW, n) = generateRandomInt w 5
                memberHealth = (partyHealth newW)!!n
                oldConditions = (partyConditions newW)
                memberConditions = oldConditions!!n
                memberName = (partyNames newW)!!n
                newWorld 
                    | memberHealth <= 0                 = noEvent newW -- party member is dead, do nothing
                    | "measles" `elem` memberConditions = noEvent newW -- party member already has measles, do nothing
                    | otherwise                              = newW {partyConditions = replaceNth oldConditions n ("measles":memberConditions), message = memberName ++ " has measles."}
                in newWorld

-- ********************** End of events **********************