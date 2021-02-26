module Date where

data Date = Date {
    day :: Int,
    month :: String,
    year :: Int
} deriving(Show)

-- Date Constructor
dateCons :: Int -> String -> Int -> Date
dateCons d m y = Date {day = d, month = m, year = y}


-- advance one day, roll over months and year
updateDate :: Date -> Date
updateDate d
    | (day d) == 31 && (month d) == "January" = Date {day = 1, month = "Feburary", year = (year d)}
    | (day d) == 28 && (month d) == "Feburary" = Date {day = 1, month = "March", year = (year d)}
    | (day d) == 31 && (month d) == "March" = Date {day = 1, month = "April", year = (year d)}
    | (day d) == 30 && (month d) == "April" = Date {day = 1, month = "May", year = (year d)}
    | (day d) == 31 && (month d) == "May" = Date {day = 1, month = "June", year = (year d)}
    | (day d) == 30 && (month d) == "June" = Date {day = 1, month = "July", year = (year d)}
    | (day d) == 31 && (month d) == "July" = Date {day = 1, month = "August", year = (year d)}
    | (day d) == 31 && (month d) == "August" = Date {day = 1, month = "September", year = (year d)}
    | (day d) == 30 && (month d) == "September" = Date {day = 1, month = "October", year = (year d)}
    | (day d) == 31 && (month d) == "October" = Date {day = 1, month = "November", year = (year d)}
    | (day d) == 30 && (month d) == "November" = Date {day = 1, month = "December", year = (year d)}
    | (day d) == 31 && (month d) == "December" = Date {day = 1, month = "January", year = (year d) + 1}
    | otherwise = Date {day = (day d) + 1, month = (month d), year = (year d)}