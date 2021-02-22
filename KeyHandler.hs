module KeyHandler where

import Definitions
import ActionHandler


-- ****************************** Top-level Key Handler Functions ******************************
handleEnterKey :: World -> World
handleEnterKey w = if screenType w == "Start"
                      then handleStartEnter w
                      else w

handleSpaceKey :: World -> World
handleSpaceKey w = if screenType w == "Start"
                      then handleStartSpace w
                      else if screenType w == "On route"
						  then handleOnRouteSpace w
						  else w

handleKey1 :: World -> World 
handleKey1 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 1 w
                       | otherwise = w
                       in newWorld

handleKey2 :: World -> World 
handleKey2 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 2 w
                       | otherwise = w
                       in newWorld

handleKey3 :: World -> World 
handleKey3 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 3 w
                       | otherwise = w
                       in newWorld

handleKey4 :: World -> World 
handleKey4 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 4 w
                       | otherwise = w
                       in newWorld

handleKey5 :: World -> World 
handleKey5 w = let newWorld
                       | screenType w == "Start" = handleStartNumbers 5 w
                       | otherwise = w
                       in newWorld

handleCtlKey :: World -> World
handleCtlKey w = let newWorld
                         | screenType w == "On route" = handleOnRouteCtl w
						 | otherwise = w
						 in newWorld