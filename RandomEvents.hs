module RandomEvents where

import Definitions
import Helpers

-- ********************** Random Event Generator **********************

randomEvent :: World -> World
randomEvent w = let (newW, n) = generateRandomInt w 100
                    newWorld
                        | n `elem` [0..49]  = noEvent newW
                        | n `elem` [50..69] = theftOxen newW
                        | n `elem` [70..84] = dysentery newW
                        | n `elem` [85..99] = cholera newW
                    in newWorld

-- ********************** End of Random Event Generator **********************


-- ********************** Events **********************

-- Each event takes in the World state and outputs the new World state
-- Each event updates the message field to contain a description of what happened,
--   to be displayed to the player. "" if no event occurred.

-- Nothing happened
noEvent :: World -> World
noEvent w = w {message = ""}

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

-- ********************** End of events **********************