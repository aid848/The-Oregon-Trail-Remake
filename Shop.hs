module Shop where

data Shop = Shop {
    store :: String,            -- e.g. "Matt's General Store"
    items :: [(String, Float)] -- e.g. [("1. Oxen", 160.00), ("2. Food", 300.00)] 
 } deriving(Show)

instance Eq Shop where
    Shop {store = s1} == Shop {store = s2} = s1 == s2


-- ***************** Functions *****************

-- Shop constructor
-- pass shop name, shop inventory, selected items
shopCons :: String -> [(String, Float)] -> Shop
shopCons st its = Shop {store = st, items = its}


-- returns price of item given item string
getItemPrice :: String -> [(String, Float)] -> Float
getItemPrice _ [] = 0.0
getItemPrice str (h:t)
    | tail (tail (tail (fst h))) == str = snd h
    | otherwise = getItemPrice str t



-- get purchase total to adjust cash balance
getPurchaseTotal :: [(String, Int, Float)] -> Float
getPurchaseTotal [] = 0.0
getPurchaseTotal (h:t) = extractThird h + getPurchaseTotal t


-- get number of oxen purchased to update oxen in World
getOxenTotal :: [(String, Int, Float)] -> Int
getOxenTotal [] = 0
getOxenTotal (h:t)
    | extractFirst h == "Oxen" = extractSecond h + getOxenTotal t
    | otherwise = getOxenTotal t

-- get amount of food purchased to update food in World
getFoodTotal :: [(String, Int, Float)] -> Int
getFoodTotal [] = 0
getFoodTotal (h:t)
    | extractFirst h == "Food" = extractSecond h + getFoodTotal t
    | otherwise = getFoodTotal t

-- get medicine amt purchased to update medicine in World
getMedicineTotal :: [(String, Int, Float)] -> Int
getMedicineTotal [] = 0
getMedicineTotal (h:t)
    | extractFirst h == "Medicine" = extractSecond h + getMedicineTotal t
    | otherwise = getMedicineTotal t

-- get clothing amt purchased to update clothign in World
getClothingTotal :: [(String, Int, Float)] -> Int
getClothingTotal [] = 0
getClothingTotal (h:t)
    | extractFirst h == "Clothing" = extractSecond h + getClothingTotal t
    | otherwise = getClothingTotal t

-- get spare parts purchased to update parts in World
getPartsTotal :: [(String, Int, Float)] -> Int
getPartsTotal [] = 0
getPartsTotal (h:t)
    | extractFirst h == "Spare Parts" = extractSecond h + getPartsTotal t
    | otherwise = getPartsTotal t


-- ***************** Helpers *****************

-- Adds item buy to s {selected} in the case where item is already present in s {selected}.  Updates total expense and amt
updateHelper :: [(String, Int, Float)] -> String -> Int -> Float -> [(String, Int, Float)]
updateHelper sel item amt price 
    | item == f a = ((f a), (s (head sel) + amt), (t (head sel) + price)) : (tail sel)
    | item == f b = a : ((f b), ((s b) + amt), ((t b) + price)) : (tail (tail sel))
    | item == f c = a : b : ((f c), ((s c) + amt), ((t c) + price)) : (tail (tail (tail sel)))
    | item == f d = a : b : c : ((f d), ((s d) + amt), ((t d) + price)) : (tail (tail (tail (tail sel))))
    | item == f e = a : b : c : d : [((f e), ((s e) + amt), ((t e) + price))]
    where
        a = (head sel)
        b = (head (tail sel))
        c = (head (tail (tail sel)))
        d = (head (tail (tail (tail sel))))
        e = (head (tail (tail (tail (tail sel)))))
        f = extractFirst
        s = extractSecond
        t = extractThird

-- transform selected to string of item names
stringItUp :: [(String, Int, Float)] -> [String]
stringItUp [] = []
stringItUp (h:t) = (extractFirst h):(stringItUp t)


extractFirst :: (String, Int, Float) -> String
extractFirst (a, _, _) = a

extractSecond :: (String, Int, Float) -> Int
extractSecond (_, b, _) = b

extractThird :: (String, Int, Float) -> Float
extractThird (_, _, c) = c