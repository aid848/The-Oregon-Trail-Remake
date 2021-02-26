module KeyHandler where

import Definitions
import ActionHandler


-- ****************************** Top-level Key Handler Functions ******************************

-- ========  Start of non-alpha-numeric keys  ========
handleEnterKey :: World -> World
handleEnterKey w = let screen = screenType w
                       newWorld
                           | screen == "Start" = handleStartEnter w
                           | screen == "Shop" = handleShopEnter w
						   | screen == "Settlement" = handleSettleEnter w
						   | screen == "Inventory" = handleInvEnter w
						   | screen == "On route" = handleOnRouteEnter w
						   | otherwise = w
						   in newWorld

handleSpaceKey :: World -> World
handleSpaceKey w = let screen = screenType w
                       newWorld
					   | screen == "Start" = handleStartSpace w
					   | screen == "On route" = handleOnRouteSpace w
					   | screen == "Shop" = handleShopSpace w
					   | screen == "Game over" = handleGameOverSpace w
					   | screen == "Inventory" = handleInvSpace w
					   | screen == "Settlement" = handleSettleSpace w
					   | screen == "River" = handleRiverSpace w
					   | screen == "Win" = handleWinSpace w
					   | otherwise = w
					   in newWorld

handleCtlKey :: World -> World
handleCtlKey w = let newWorld
                         | screenType w == "On route" = handleOnRouteCtl w
						 | otherwise = w
						 in newWorld

-- ========  End of non-alpha-numeric keys  ========


-- ========  Start of alpha-numeric keys  ========

handleKey0 :: World -> World 
handleKey0 w = let newWorld
					   | screenType w == "Shop" = handleShopNumbers 0 w
					   | screenType w == "Settlement" = handleSettleNumbers 0 w
					   | screenType w == "Inventory" = handleInvNumbers 0 w
					   | otherwise  = w
					   in newWorld

handleKey1 :: World -> World 
handleKey1 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 1 w
					   | screenType w == "Shop" = handleShopNumbers 1 w
					   | screenType w == "Inventory" = handleInvNumbers 1 w
					   | screenType w == "Settlement" = handleSettleNumbers 1 w
					   | screenType w == "River" = handleRiverNumbers 1 w
                       | otherwise = w
                       in newWorld

handleKey2 :: World -> World 
handleKey2 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 2 w
					   | screenType w == "Shop" = handleShopNumbers 2 w
					   | screenType w == "Inventory" = handleInvNumbers 2 w
					   | screenType w == "Settlement" = handleSettleNumbers 2 w
					   | screenType w == "River" = handleRiverNumbers 2 w
                       | otherwise = w
                       in newWorld

handleKey3 :: World -> World 
handleKey3 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 3 w
					   | screenType w == "Shop" = handleShopNumbers 3 w
					   | screenType w == "Inventory" = handleInvNumbers 3 w
					   | screenType w == "Settlement" = handleSettleNumbers 3 w
					   | screenType w == "River" = handleRiverNumbers 3 w
                       | otherwise = w
                       in newWorld

handleKey4 :: World -> World 
handleKey4 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 4 w
					   | screenType w == "Shop" = handleShopNumbers 4 w
					   | screenType w == "Inventory" = handleInvNumbers 4 w
					   | screenType w == "Settlement" = handleSettleNumbers 4 w
                       | otherwise = w
                       in newWorld

handleKey5 :: World -> World 
handleKey5 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 5 w
					   | screenType w == "Shop" = handleShopNumbers 5 w
					   | screenType w == "Inventory" = handleInvNumbers 5 w
					   | screenType w == "Settlement" = handleSettleNumbers 5 w
                       | otherwise = w
                       in newWorld

handleKey6 :: World -> World
handleKey6 w = let newWorld
                       | screenType w == "Inventory" = handleInvNumbers 6 w
					   | screenType w == "Settlement" = handleSettleNumbers 6 w
					   | screenType w == "Shop" = handleShopNumbers 6 w
					   | otherwise = w
					   in newWorld

handleKey7 :: World -> World
handleKey7 w = let newWorld
                       | screenType w == "Inventory" = handleInvNumbers 7 w
					   | screenType w == "Settlement" = handleSettleNumbers 7 w
					   | screenType w == "Shop" = handleShopNumbers 7 w
					   | otherwise = w
					   in newWorld

handleKey8 :: World -> World
handleKey8 w = let newWorld
                       | screenType w == "Inventory" = handleInvNumbers 8 w
					   | screenType w == "Settlement" = handleSettleNumbers 8 w
					   | screenType w == "Shop" = handleShopNumbers 8 w
					   | otherwise = w
					   in newWorld

handleKey9 :: World -> World
handleKey9 w = let newWorld
                       | screenType w == "Inventory" = handleInvNumbers 9 w
					   | screenType w == "Settlement" = handleSettleNumbers 9 w
					   | screenType w == "Shop" = handleShopNumbers 9 w
					   | otherwise = w
					   in newWorld

handleCharA :: World -> World 
handleCharA w = let newWorld
                        | screenType w == "Start" = handleStartChar "A" w
						| otherwise = w
						in newWorld

handleCharB :: World -> World 
handleCharB w = let newWorld
                        | screenType w == "Start" = handleStartChar "B" w
						| otherwise = w
						in newWorld

handleCharC :: World -> World 
handleCharC w = let newWorld
                        | screenType w == "Start" = handleStartChar "C" w
						| otherwise = w
						in newWorld

handleCharD :: World -> World 
handleCharD w = let newWorld
                        | screenType w == "Start" = handleStartChar "D" w
						| otherwise = w
						in newWorld

handleCharE :: World -> World 
handleCharE w = let newWorld
                        | screenType w == "Start" = handleStartChar "E" w
						| otherwise = w
						in newWorld

handleCharF :: World -> World 
handleCharF w = let newWorld
                        | screenType w == "Start" = handleStartChar "F" w
						| otherwise = w
						in newWorld

handleCharG :: World -> World 
handleCharG w = let newWorld
                        | screenType w == "Start" = handleStartChar "G" w
						| otherwise = w
						in newWorld

handleCharH :: World -> World 
handleCharH w = let newWorld
                        | screenType w == "Start" = handleStartChar "H" w
						| otherwise = w
						in newWorld

handleCharI :: World -> World 
handleCharI w = let newWorld
                        | screenType w == "Start" = handleStartChar "I" w
						| otherwise = w
						in newWorld

handleCharJ :: World -> World 
handleCharJ w = let newWorld
                        | screenType w == "Start" = handleStartChar "J" w
						| otherwise = w
						in newWorld

handleCharK :: World -> World 
handleCharK w = let newWorld
                        | screenType w == "Start" = handleStartChar "K" w
						| otherwise = w
						in newWorld

handleCharL :: World -> World 
handleCharL w = let newWorld
                        | screenType w == "Start" = handleStartChar "L" w
						| otherwise = w
						in newWorld

handleCharM :: World -> World 
handleCharM w = let newWorld
                        | screenType w == "Start" = handleStartChar "M" w
						| otherwise = w
						in newWorld

handleCharN :: World -> World 
handleCharN w = let newWorld
                        | screenType w == "River" = handleRiverChar 'N' w
						| screenType w == "Start" = handleStartChar "N" w
						| otherwise = w
						in newWorld
						
handleCharO :: World -> World 
handleCharO w = let newWorld
                        | screenType w == "Start" = handleStartChar "O" w
						| otherwise = w
						in newWorld

handleCharP :: World -> World 
handleCharP w = let newWorld
                        | screenType w == "Start" = handleStartChar "P" w
						| otherwise = w
						in newWorld

handleCharQ :: World -> World 
handleCharQ w = let newWorld
                        | screenType w == "Start" = handleStartChar "Q" w
						| otherwise = w
						in newWorld

handleCharR :: World -> World 
handleCharR w = let newWorld
                        | screenType w == "Start" = handleStartChar "R" w
						| otherwise = w
						in newWorld

handleCharS :: World -> World 
handleCharS w = let newWorld
                        | screenType w == "Start" = handleStartChar "S" w
						| otherwise = w
						in newWorld

handleCharT :: World -> World 
handleCharT w = let newWorld
                        | screenType w == "Start" = handleStartChar "T" w
						| otherwise = w
						in newWorld

handleCharU :: World -> World 
handleCharU w = let newWorld
                        | screenType w == "Start" = handleStartChar "U" w
						| otherwise = w
						in newWorld

handleCharW :: World -> World 
handleCharW w = let newWorld
                        | screenType w == "Start" = handleStartChar "W" w
						| otherwise = w
						in newWorld

handleCharV :: World -> World 
handleCharV w = let newWorld
                        | screenType w == "Start" = handleStartChar "V" w
						| otherwise = w
						in newWorld

handleCharX :: World -> World 
handleCharX w = let newWorld
                        | screenType w == "Start" = handleStartChar "X" w
						| otherwise = w
						in newWorld


handleCharY :: World -> World 
handleCharY w = let newWorld
                        | screenType w == "River" = handleRiverChar 'Y' w
						| screenType w == "Start" = handleStartChar "Y" w
						| otherwise = w
						in newWorld

handleCharZ :: World -> World 
handleCharZ w = let newWorld
                        | screenType w == "Start" = handleStartChar "Z" w
						| otherwise = w
						in newWorld

