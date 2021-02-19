module Shop where

data Shop = Shop {
    store :: String,            -- e.g. "Matt's General Store"
    items :: [(String, Float)], -- e.g. [("1. Oxen", 160.00), ("2. Food", 300.00)]
    selected :: [(String, Int, Float)]  -- e.g. [("Oxen", 2, 320.00)]
} deriving(Show)

instance Eq Shop where
    Shop {store = s1} == Shop {store = s2} = s1 == s2

-- Shop constructor
shopCons :: String -> [(String, Float)] -> [(String, Int, Float)] -> Shop
shopCons st its slct = Shop {store = st, items = its, selected = slct}



-- Update selected items to purchase
-- Handles cases: - selected is empty 
--                - selected is non-empty and does not contain item
--                - selected is non-empty and does contain item
--
-- updateSelected(shop, item, #item, item price)
updateSelected :: Shop -> String -> Int -> Float-> Shop
updateSelected s item amt price
    | (selected s) == [] = s {selected = [(item, amt, price)]}
    | item `elem` stock = s {selected = (updateHelper (selected s) item amt price)} 
    | otherwise = s {selected = (item, amt, price):(selected s)}
    where 
        stock = stringItUp (selected s) 


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