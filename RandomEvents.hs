module RandomEvents where

import System.Random
import Main
import Test

-- Helpers

-- generateRandomInt lower upper
-- Takes a lower bound and an upper bound and generates an IO Int n, lower <= n <= upper
generateRandomInt :: Int -> Int -> IO Int
generateRandomInt lower upper = randomRIO (lower :: Int, upper)

-- replaceNth arr n new 
-- Replaces the nth element of arr with new
replaceNth :: [a] -> Int -> a -> [a]
replaceNth [] _ new = []
replaceNth (h:t) 0 new = new:t
replaceNth (h:t) n new = h:(replaceNth t (n-1) new)

-- List of events
-- Each event takes in the World state and outputs the new IO World state
-- Each event updates the message field to contain a description of what happened,
--   to be displayed to the player

-- Reduces world.oxen by a random number between 1 and (world.oxen - 1)
theftOxen :: World -> IO World
theftOxen w 
    | (oxen w) == 1 = return w -- there is only 1 oxen left, do nothing
    | otherwise     = 
        do
            let numOxen = (oxen w)
            n <- generateRandomInt 1 (numOxen - 1)
            return w {oxen = numOxen - n, message = "A thief comes during the night and steals " ++ show n ++ " oxen."}

-- Gives the "dysentery" condition to a random party member
dysentery :: World -> IO World
dysentery w
    = do
        n <- generateRandomInt 0 4
        let partyMemberHealth = (partyHealth w)!!n
        let oldConditions = (partyConditions w)
        let partyMemberConditions = oldConditions!!n
        let newWorld | partyMemberHealth == 0                   = w -- party member is dead, do nothing
                     | "dysentery" `elem` partyMemberConditions = w -- party member already has dysentery, do nothing
                     | otherwise                                = w {partyConditions = replaceNth oldConditions n ("dysentery":partyMemberConditions), message = "Party member " ++ show n ++ " has dysentery."}
        return newWorld

-- Gives the "cholera" condition to a random party member
cholera :: World -> IO World
cholera w
    = do
        n <- generateRandomInt 0 4
        let partyMemberHealth = (partyHealth w)!!n
        let oldConditions = (partyConditions w)
        let partyMemberConditions = oldConditions!!n
        let newWorld | partyMemberHealth == 0                 = w -- party member is dead, do nothing
                     | "cholera" `elem` partyMemberConditions = w -- party member already has cholera, do nothing
                     | otherwise                              = w {partyConditions = replaceNth oldConditions n ("cholera":partyMemberConditions), message = "Party member " ++ show n ++ " has cholera."}
        return newWorld