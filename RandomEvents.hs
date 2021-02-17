module RandomEvents where

import Definitions

-- Helpers

-- generateRandomInt w lower upper
-- Takes in a world state and an upper bound and returns a new world state and a random Int n, 0 <= n < upper
-- This function is based on the rngSeed field in the world state and will update said field in the new world state
generateRandomInt :: World -> Int -> (World, Int)
generateRandomInt w upper = let seed = (rngSeed w)
                                a = 1103515245
                                c = 12345
                                m = 2^32
                                newSeed = (a*seed + c) `mod` m
                                in ((w {rngSeed = newSeed}), newSeed `mod` upper)

-- replaceNth arr n new 
-- Replaces the nth element of arr with new
replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ new = []
replaceNth (h:t) 0 new = new:t
replaceNth (h:t) n new = h:(replaceNth t (n-1) new)

-- ********************** Events **********************

-- Each event takes in the World state and outputs the new World state
-- Each event updates the message field to contain a description of what happened,
--   to be displayed to the player. "" if no event occurred.

-- Reduces world.oxen by a random number between 1 and (world.oxen - 1)
theftOxen :: World -> World
theftOxen w 
    | (oxen w) == 1 = w {message = ""} -- there is only 1 oxen left, do nothing
    | otherwise     = let numOxen = (oxen w)
                          (newW, n) = generateRandomInt w (numOxen - 1)
                          scaledN = n + 1
                          in newW {oxen = numOxen - scaledN, message = "A thief comes during the night and steals " ++ show scaledN ++ " oxen."}

-- Gives the "dysentery" condition to a random party member
dysentery :: World -> World
dysentery w = let (newW, n) = generateRandomInt w 5
                  partyMemberHealth = (partyHealth newW)!!n
                  oldConditions = (partyConditions newW)
                  partyMemberConditions = oldConditions!!n
                  partyMemberName = (partyNames newW)!!n
                  newWorld 
                    | partyMemberHealth == 0                   = newW {message = ""} -- party member is dead, do nothing
                    | "dysentery" `elem` partyMemberConditions = newW {message = ""} -- party member already has dysentery, do nothing
                    | otherwise                                = newW {partyConditions = replaceNth oldConditions n ("dysentery":partyMemberConditions), message = partyMemberName ++ " has dysentery."}
                  in newWorld

-- Gives the "cholera" condition to a random party member
cholera :: World -> World
cholera w = let (newW, n) = generateRandomInt w 5
                partyMemberHealth = (partyHealth newW)!!n
                oldConditions = (partyConditions newW)
                partyMemberConditions = oldConditions!!n
                partyMemberName = (partyNames newW)!!n
                newWorld 
                    | partyMemberHealth == 0                 = newW {message = ""} -- party member is dead, do nothing
                    | "cholera" `elem` partyMemberConditions = newW {message = ""} -- party member already has cholera, do nothing
                    | otherwise                              = newW {partyConditions = replaceNth oldConditions n ("cholera":partyMemberConditions), message = partyMemberName ++ " has cholera."}
                in newWorld

-- ********************** End of events **********************