module ActionHandler where

import Definitions
import Helpers
import Map
import Shop


-- Constants

restDayHealthIncrease = 1


-- Handler Functions

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


-- replaceNth arr n new 
-- Replaces the nth element of arr with new