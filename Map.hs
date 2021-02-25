module Map where
import Shop
import Date

-- TODO: showing current distance, distance remaining, check reached next destination
--         - implement after discussion on location of distance values

data Node = Node {
  name :: String,
  river :: Bool,    -- True if river
  depth :: Int,     -- - river params
  width :: Int,     -- -
  next :: Upcoming, -- (Node, distance tuples).  Distances indicate distance from THIS node
  dist :: Int,      -- distance travelled from THIS node, e.g., 0 <= dist <= chosen node in Upcoming
  shop :: Shop
} deriving(Eq, Show)



-- map nodes ahead on the trail: list of (name, distance) tuples or Empty
data Upcoming = Empty
            | Dests [(Node, Int)]

instance Eq Upcoming where 
    Dests (h:t) == Empty = False
    Empty ==  Empty = True
    Dests [(n1, i1)] == Dests [(n2, i2)] = n1 == n2

instance Show Upcoming where
    show Empty = "Empty"
    show (Dests [(n, d)]) = show (name n) ++ show d
    show (Dests [(n, d), (n1,d1)]) = show (name n) ++ " " ++ show d ++ "  " ++ show (name n) ++ " " ++ show d




-- A test Shop
shop0 :: Shop
shop0 = shopCons "Matt's General Store" [("1. Oxen", 100.00), ("2. Food", 10.00), ("3. Spare Parts", 50.00), ("4. Clothing", 25.00), ("5. Medicine", 30.00)]


{- 
define a short test path:

											---33----- wandering brook
											|						|
											|						15
											|						|
buffalo head---22---blue river-----13-----salmon run-----50----- gold gulch
						|											|
						|											|
						--------30------ red ridge ------ 41 ------
-}

-- A test Date
date0 :: Date
date0 = dateCons 1 "Feburary" 1848

gold_gulch :: Node
gold_gulch = Node {name = "Denver", river = False, depth = 0, width = 0, next = Empty, dist = 0, shop = shop0}

wandering_brook :: Node
wandering_brook = Node {name = "Wandering Brook", river = True, depth = 9, width = 300, next = Dests [(gold_gulch, 20)], dist = 0, shop=shop0}

salmon_run :: Node
salmon_run = Node {name = "Salmon Run", river = False, depth = 0, width = 0, next = Dests [(gold_gulch, 50), (wandering_brook, 30)], dist = 0, shop = shop0}

red_ridge :: Node
red_ridge = Node {name = "Red Ridge", river = False, depth = 0, width = 0, next = Dests [(gold_gulch, 40)], dist = 0, shop = shop0}

blue_river :: Node
blue_river = Node {name = "Blue River", river = True, depth = 50, width = 200, next = Dests [(salmon_run, 30), (red_ridge, 50)], dist = 0, shop = shop0}

buffalo_head :: Node
buffalo_head = Node {name = "Buffalo Head", river = False, depth = 0, width = 0, next = Dests [(blue_river, 40)], dist = 0, shop = shop0}


-- a map is a list of nodes
type Map = [Node]

-- test function showing node names in map
-- showMap :: Map -> String
-- showMap [] = " "
-- showMap (h:t) = show (name h) ++ " " ++ showMap t


checkRiver :: Node -> Bool
checkRiver n = river n



-- !! for Aidan 
-- get distance to next landmark
-- takes in currentLocation and nextLocation nodes from WS
distToLandmark:: Node -> Node -> Int
distToLandmark curr nextLoc = let pos = dist curr
                                  toGo
                                      | (name curr) == (name nextLoc) = 0
                                      | (name nextLoc) == (name (getFirstInNext curr)) = (getFirstDistInNext curr) - pos
                                      | otherwise = (getSecondDistInNext curr) - pos
                                  in toGo

-- returns list of itemized next node names, of length 1 or 2 
getBranchNames :: Node -> [String]
getBranchNames n = let names = []
                       locs
                           | not (hasBranch n) = first : names
                           | otherwise = first : second : names
                           in locs where
                               first = "1. " ++ name (fst (head (upcomingToList (next n))))
                               second = "2. " ++ name (fst (head (tail (upcomingToList (next n)))))

-- checks to see if we've reached the next node, pass in currentLocation and nextLocation in node
reachedNext :: Node -> Node -> Bool
reachedNext current next = let x = dist current
                               y = dist next
                               in x == y


-- checks to see if next field is Empty
-- returns True if Empty
isNextEmpty :: Node -> Bool
isNextEmpty n
    | upcomingToList (next n) == [] = True
    | otherwise = False

-- checks to see if node's next field contains a branch (two tuples)
-- returns True if branch
hasBranch :: Node -> Bool
hasBranch n
    | tail (upcomingToList (next n)) == [] = False
    | otherwise = True


-- returns first Node in next of current Node
-- assumes not Empty
getFirstInNext :: Node -> Node
getFirstInNext n = fst (head (upcomingToList (next n)))

-- returns second Node in next of current node
-- assumes not empty
getSecondInNext :: Node -> Node
getSecondInNext n = fst (head (tail (upcomingToList (next n))))

getFirstDistInNext :: Node -> Int
getFirstDistInNext n = snd (head (upcomingToList (next n)))

getSecondDistInNext :: Node -> Int
getSecondDistInNext n = snd (head (tail (upcomingToList (next n))))


-- returns Upcoming field as list
upcomingToList :: Upcoming -> [(Node, Int)]
upcomingToList u
    | u == Empty = []
    | otherwise = upcomingToListHelper u 

-- helper to extract list from Upcoming
upcomingToListHelper :: Upcoming -> [(Node, Int)]
upcomingToListHelper (Dests (h:t)) = (h:t)
