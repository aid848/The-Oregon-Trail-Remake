module Shop where

import Date

data Shop = Shop {
    store :: String,            -- e.g. "Matt's General Store"
    date :: Date,               -- data Date: e.g. "March 1, 1848"
    items :: [(String, Float)], -- e.g. [("1. Oxen", 160.00), ("2. Food", 300.00)]
    message :: String,          -- e.g. "Which item would you like to buy?"
    exit :: String              -- e.g. "Press SPACE BAR to leave store"
} deriving(Show)

instance Eq Shop where
    Shop {store = s1} == Shop {store = s2} = s1 == s2

-- Shop constructor
shopCons :: String -> Date -> [(String, Float)] -> String -> String -> Shop
shopCons st dt its msg ext = Shop {store = st, date = dt, items = its, message = msg, exit = ext}


