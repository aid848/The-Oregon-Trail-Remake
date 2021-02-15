module ScreenHandler where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Definitions
drawScreen w = Color white (Pictures [Translate (-600) (300) (Scale 0.25 0.25 (Text (message w))),
 Translate (-600) (250) (Scale 0.25 0.25 (Text (message w)))] )