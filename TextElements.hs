module TextElements where

-- store gui text

generalStoreHeader :: String
generalStoreHeader = "Matt's General Store"

storeLeave :: String
storeLeave = "Press Space Bar to leave store"


-- settlement actions
settleContinue :: String
settleContinue = "1. Continue on the trail"
settleCheck :: String
settleCheck = "2. Check supplies"
settleMap:: String
settleMap = "3. Look at map"
settleChangePace :: String
settleChangePace = "4. Change pace"
settleFood :: String
settleFood = "5. Change food rations"
settleRest :: String
settleRest = "6. Stop to rest"
-- settleTrade :: String
-- settleTrade = "7. Attempt to trade"
-- settleTalk :: String -- not enough time for it to be worth it?
-- settleTalk = "8. Talk to people"
-- invFood :: String
-- invFood = "8. Hunt for food"
settleShop :: String
settleShop = "7. Buy supplies"

baseActionText :: [String]
baseActionText = ["You may:",settleContinue,settleCheck,settleMap,settleChangePace,settleFood,settleRest]

invActionsText :: [String]
invActionsText = baseActionText
settleActionsText :: [String]
settleActionsText = baseActionText++[settleShop]


-- inventory items
invItemText :: [String]
invItemText = ["Oxen","Pounds of food","Spare parts","Sets of clothing", "Doses of Medicine","Money Left"]

paceChangeInfoText :: [String]
paceChangeInfoText = ["The pace at which you travel can change.", "        Your choices are:       ", "        1. a steady pace      ", "        2. a strenuous pace      ", "        3. a grueling pace      "]


foodInfoText :: String
foodInfoText = "The amount of food the people in   your party eat each day can change.These amounts are:"

foodFillingText :: String
foodFillingText = "1. filling - meals are large and generous."

foodMeagerText :: String
foodMeagerText = "2. meager - meals are small, but adequate."

foodBareText :: String
foodBareText = "3. bare bones - meals are very small;"

foodChoicesText :: [String]
foodChoicesText = [foodFillingText, foodMeagerText,foodBareText,"            everyone stays hungry."]

introWealth :: String
introWealth = "Many kinds of people made the trip to Oregon."

introWealthOptionsText :: [String]
introWealthOptionsText = ["You may:", "1. Be a banker from Boston", "2. Be a carpenter from Ohio", "3. Be a farmer from Illinois"]

introWagonLeader :: String
introWagonLeader = "What is the first name of the wagon leader?    "

otherPartyMembers :: String
otherPartyMembers = "What are the first names of the four other members in your party?"

oneToFive :: [String]
oneToFive = ["1.","2.","3.","4.","5."]

introLocationInfo :: String
introLocationInfo = "It is 1848. Your starting place for Oregon is Independence Missouri.  You must decide which month to leave Independence."

introLocationMonths :: [String]
introLocationMonths = ["1. March","2. April", "3. May", "4. June", "5. July"]

introOutTextOne :: String
introOutTextOne = "Before leaving Independence you should buy equipment and supplies. youhave $"

introOutTextTwo :: String
introOutTextTwo = " in cash, but you don't have to spend it all now."

introShopWarning :: String
introShopWarning = "Make sure to visit the shop before heading out!"

riverOptions :: [String]
riverOptions = ["You may:", "1. attempt to ford the river", "2. caulk the wagon and float it across", "3. take a ferry across"]

riverInfoOne :: String
riverInfoOne = "You must cross the river in order to continue. The river at this point is currently "

riverFerryInfo :: String
riverFerryInfo = "The ferry operator says that he will charge you $5.00 and that you    will have to wait 5 days."

riverFloatInfo :: String
riverFloatInfo = "You decide to float the wagon across. Your wagon and party make it    across without issue."

riverFord :: String
riverFord = "You decide to take your wagon across the river. Your wagon and party  make it across without issue."

riverPayment :: String
riverPayment = "Are you willing to do this?"

oxenInfo :: String
oxenInfo = "Oxen are used to pull your wagon. Make sure to have enough in case of any accidents or disease"
foodInfo :: String
foodInfo = "You'll need enough food to make it on your trip. Make sure to have    enough to at least make it to the next settlement!"
sparePartsInfo :: String
sparePartsInfo = "Your wagon may be damaged on your trip. Having some spare parts for  repairs is important!"
clothingInfo :: String
clothingInfo = "Your clothing will wear out on the trip. Having extra sets will keep  you and your family warm and healthy."
medicineInfo :: String
medicineInfo = "Staying healthy and fighting off illness will be important for your   trip. Make sure to have enough medicine!"