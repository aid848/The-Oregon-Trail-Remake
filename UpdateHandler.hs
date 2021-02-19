module UpdateHandler where

import Definitions
import Date
import RandomEvents
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

-- ********************** End of constants **********************

-- update w
-- Takes in a world and steps forward by one day, causing all the effects necessary:
--   - (Possibly) generate a random event
-- Returns the updated world
update :: World -> World
update w = let newW = (randomEvent (applyPace (applyRationing (applyPartyConditions w))))
               oldDate = date newW
               in newW {date = updateDate oldDate}

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