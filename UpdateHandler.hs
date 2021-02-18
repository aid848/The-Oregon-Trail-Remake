module UpdateHandler where

import Definitions
import RandomEvents
import Helpers

dysenteryHealthDrain = 5
choleraHealthDrain = 5

-- update w
-- Takes in a world and steps forward by one day, causing all the effects necessary:
--   - (Possibly) generate a random event
-- Returns the updated world
update :: World -> World
update w = (randomEvent (applyPartyConditions w))

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
                            | "dysentery" `elem` memberConditions = w {partyHealth = replaceNth partyHealths n (partyHealths!!n - dysenteryHealthDrain)}
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
                        | "cholera" `elem` memberConditions = w {partyHealth = replaceNth partyHealths n (partyHealths!!n - choleraHealthDrain)}
                        | otherwise                         = w
                       in newWorld