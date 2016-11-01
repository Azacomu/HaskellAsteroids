module MenuView where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle
import Data.Monoid

import Control.Lens
import Helper

import Model

drawMenu :: Float -> Float -> World -> Picture
drawMenu horizontalResolution verticalResolution world
    = translate left top $ scaleBoth (worldScale) $ color white $
             (translate 10 (textBaseH * (-0.5) - 10) $
                  scaleBoth 0.5 $ text "Hasteroids")
          <> drawOptions
         --(color white $ translate 10 (textBaseH * (-0.8) - 20) $ scaleBoth 0.3 $ text ">Play")
    where worldScale  = verticalResolution / 576
          top         = verticalResolution / 2
          left        = horizontalResolution / (-2)
          textBaseH   = 110
          options     = ["Play", "Quit"]
          drawOptions =
               fst $ foldl (\(picture, i) option ->
                                 (picture
                                  <> (translate 10
                                                (-10
                                                 - textBaseH * (0.5 + 0.3 * i)
                                                 - 10 * i)
                                                $ scaleBoth 0.3 $ text option),
                                  i + 1))
                           (blank, 1)
                           options