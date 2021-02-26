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
define a map:
                                                                                                                                            ---------208------ white willow ------
                                                                    ---300----- wandering brook                                             |                                    |
                                                                    |						|                                               185                                  |
                                                                    |						175                                             |                                   96
                                                                    |						|                                               |                                    |
independence-----150----buffalo head---400---blue river-----90-----salmon run              gold gulch ---90----rushing rapids -----150---- purple canyon                       victory village
                                                                    |											|                           |                                      |
                                                                    |											|                           |                                      |
                                                                    --------220------ red ridge ------ 100 ------                            --------------310------ pesky pass----41
-}

-- Node Constructor
-- nodeCons name river depth width Upcoming dist shop
nodeCons :: String -> Bool -> Int -> Int -> Upcoming -> Int -> Shop -> Node
nodeCons n riv d w u dis s = Node {name = n, river = riv, depth = d, width = w, next = u, dist = dis, shop = s}


-- Oregon Trail Map Nodes:
independence :: Node
independence = nodeCons "Independence" False 0 0 (Dests [(fortKearney, 150)]) 0 shop0

fortKearney :: Node
fortKearney = nodeCons "Fort Kearney" False 0 0 (Dests [(riverOfDoubt, 98)]) 0 shop0

riverOfDoubt :: Node
riverOfDoubt = nodeCons "River of Doubt" True 25 501 (Dests [(laramie, 85)]) 0 shop0

laramie :: Node
laramie = nodeCons "Laramie" False 0 0 (Dests [(rushingRapids, 195)]) 0 shop0

rushingRapids :: Node
rushingRapids = nodeCons "Rushing Rapids" True 18 350 (Dests [(southPass, 300)]) 0 shop0

southPass :: Node
southPass = nodeCons "South Pass" False 0 0 (Dests [(sodaSprings, 220), (fortBridger, 289)]) 0 shop0

sodaSprings :: Node
sodaSprings = nodeCons "Soda Springs" False 0 0 (Dests [(fortHall, 109)]) 0 shop0

fortBridger :: Node
fortBridger = nodeCons "Fort Bridger" False 0 0 (Dests [(fortHall, 130)]) 0 shop0

fortHall :: Node
fortHall = nodeCons "Fort Hall" False 0 0 (Dests [(rockyRiver, 280)]) 0 shop0

rockyRiver :: Node
rockyRiver = nodeCons "Rocky River" True 36 190 (Dests [(fortBoise, 392)]) 0 shop0

fortBoise :: Node
fortBoise = nodeCons "Fort Boise" False 0 0 (Dests [(fortWallaWalla, 175), (blueMountain, 206)]) 0 shop0

fortWallaWalla :: Node
fortWallaWalla = nodeCons "Fort Walla Walla" False 0 0 (Dests [(blackRiver, 120)]) 0 shop0

blueMountain :: Node
blueMountain = nodeCons "Blue Mountain" False 0 0 (Dests [(redRiver, 160)]) 0 shop0

blackRiver :: Node
blackRiver = nodeCons "Black River" True 84 100 (Dests [(oregonCity, 290)]) 0 shop0

redRiver :: Node
redRiver = nodeCons "Red River" True 8 250 (Dests [(oregonCity, 304)]) 0 shop0

oregonCity :: Node
oregonCity = nodeCons "Oregon City" False 0 0 Empty 0 shop0


-- a map is a list of nodes
-- type Map = [Node]

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
    | null (upcomingToList (next n))= True
    | otherwise = False

-- checks to see if node's next field contains a branch (two tuples)
-- returns True if branch
hasBranch :: Node -> Bool
hasBranch n
    | null (tail (upcomingToList (next n))) = False
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
upcomingToListHelper (Dests (h:t)) = h : t
