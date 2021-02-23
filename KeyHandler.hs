module KeyHandler where

import Definitions
import ActionHandler


-- ****************************** Top-level Key Handler Functions ******************************
handleEnterKey :: World -> World
handleEnterKey w = if screenType w == "Start"
                      then handleStartEnter w
                      else if screenType w == "Shop"
						  then handleShopEnter w
						  else w

handleSpaceKey :: World -> World
handleSpaceKey w = if screenType w == "Start"
                      then handleStartSpace w
                      else if screenType w == "On route"
						  then handleOnRouteSpace w
						  else if screenType w == "Shop"
							  then handleShopSpace w
							  else if screenType w == "Game over"
								  then handleGameOverSpace w
								  else if screenType w == "Inventory"
									  then handleInvSpace w
									  else if screenType w == "Settlement"
										  then handleSettleSpace w
										  else w

handleKey1 :: World -> World 
handleKey1 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 1 w
					   | screenType w == "Shop" = handleShopNumbers 1 w
					   | screenType w == "Inventory" = handleInvNumbers 1 w
					   | screenType w == "Settlement" = handleSettleNumbers 1 w
                       | otherwise = w
                       in newWorld

handleKey2 :: World -> World 
handleKey2 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 2 w
					   | screenType w == "Shop" = handleShopNumbers 2 w
					   | screenType w == "Inventory" = handleInvNumbers 2 w
					   | screenType w == "Settlement" = handleSettleNumbers 2 w
                       | otherwise = w
                       in newWorld

handleKey3 :: World -> World 
handleKey3 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 3 w
					   | screenType w == "Shop" = handleShopNumbers 3 w
					   | screenType w == "Inventory" = handleInvNumbers 3 w
					   | screenType w == "Settlement" = handleSettleNumbers 3 w
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
					   | otherwise = w
					   in newWorld

handleKey7 :: World -> World
handleKey7 w = let newWorld
                       | screenType w == "Inventory" = handleInvNumbers 7 w
					   | screenType w == "Settlement" = handleSettleNumbers 7 w
					   | otherwise = w
					   in newWorld

handleKey8 :: World -> World
handleKey8 w = let newWorld
                       | screenType w == "Inventory" = handleInvNumbers 8 w
					   | screenType w == "Settlement" = handleSettleNumbers 8 w
					   | otherwise = w
					   in newWorld

handleKey9 :: World -> World
handleKey9 w = let newWorld
                       | screenType w == "Inventory" = handleInvNumbers 9 w
					   | otherwise = w
					   in newWorld

handleCtlKey :: World -> World
handleCtlKey w = let newWorld
                         | screenType w == "On route" = handleOnRouteCtl w
						 | otherwise = w
						 in newWorld