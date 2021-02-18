module Helpers where

import Definitions

-- ********************** Helpers **********************

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

-- ********************** End of Helpers **********************