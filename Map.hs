module Map where
import Shop
import Date

data Node = Node {
  name :: String,
  next :: Upcoming,
  dist :: Int,
  shop :: Shop
} deriving(Eq, Show)


-- map nodes ahead on the trail: list of (name, distance) tuples or Empty
data Upcoming = Empty
            | Dests [(Node, Int)]

instance Eq Upcoming where 
    Dests [(n1, i1)] == Empty = False
    Empty ==  Empty = True
    Dests [(n1, i1)] == Dests [(n2, i2)] = n1 == n2

instance Show Upcoming where
    show Empty = "Empty"
    show (Dests [(n, d)]) = show (name n) ++ show d





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

-- A test Shop
shop0 :: Shop
shop0 = shopCons "Matt's General Store" date0 [("1. Oxen", 160.00), ("2. Food", 300.00), ("3. Clothes", 100.00), ("4. Guns", 50.00), ("2. Spare Parts", 20.00)] 
        "Which item would you like to buy?" "Press SPACE BAR to leave the store"

gold_gulch :: Node
gold_gulch = Node {name = "Denver", next = Empty, dist = 0, shop = shop0}

wandering_brook :: Node
wandering_brook = Node {name = "Wandering Brook", next = Dests [(gold_gulch, 15)], dist = 0, shop=shop0}

salmon_run :: Node
salmon_run = Node {name = "Salmon Run", next = Dests [(gold_gulch, 50), (wandering_brook, 33)], dist = 0, shop = shop0}

red_ridge :: Node
red_ridge = Node {name = "Red Ridge", next = Dests [(gold_gulch, 41)], dist = 0, shop = shop0}

blue_river :: Node
blue_river = Node {name = "Blue River", next = Dests [(salmon_run, 13), (red_ridge, 30)], dist = 0, shop = shop0}

buffalo_head :: Node
buffalo_head = Node {name = "Buffalo Head", next = Dests [(blue_river, 22)], dist = 0, shop = shop0}


-- a map is a list of nodes
type Map = [Node]


-- map constructor for straight paths
mapCons :: Node -> Map
mapCons node
    | next node == Empty = [node]
    | otherwise = node : (mapCons (getFirstInNext node))

-- test function showing node names in map
-- showMap :: Map -> String
-- showMap [] = " "
-- showMap (h:t) = show (name h) ++ " " ++ showMap t


-- checks to see if next field is empty
isNextEmpty :: Node -> Bool
isNextEmpty n
    | upcomingToList (next n) == [] = True
    | otherwise = False

-- returns first Node in next of current Node
-- assumes not Empty
getFirstInNext :: Node -> Node
getFirstInNext n = fst (head (upcomingToList (next n)))

-- returns second Node in next of current node
-- assumes not empty
getSecondInNext :: Node -> Node
getSecondInNext n = fst (head (tail (upcomingToList (next n))))

-- returns distance to upcoming node given (next node') where node' is current node
getDist :: Upcoming -> Int
getDist (Dests [(n, d)]) = d 

-- todo
-- checks to see if we've reached the next node, can select first or second option in Upcoming
-- if select is 1, check first node in next, if 2, check second node in next
-- reachedNext :: Node -> Int -> Bool
-- reachedNext n sel
--     | sel == 1 = checkDistance n (getFirstInNext n)


-- returns Upcoming field as list
upcomingToList :: Upcoming -> [(Node, Int)]
upcomingToList u
    | u == Empty = []
    | otherwise = upcomingToListHelper u 

-- helper to extract list from Upcoming
upcomingToListHelper :: Upcoming -> [(Node, Int)]
upcomingToListHelper (Dests [(n, d)]) = [(n, d)]







