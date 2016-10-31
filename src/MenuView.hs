module MenuView where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle
import Data.Monoid

import Control.Lens

import Model

drawMenu :: Float -> Float -> World -> Picture
drawMenu horizontalResolution verticalResolution world
    = scale 0.3 0.3 $ translate 0 0 $ color green $ text "Welcome!"