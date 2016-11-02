module MenuView where

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle
import Data.Monoid

import Control.Lens
import Helper
import Controller.MenuUpdate

import Model

drawMenu :: Float -> Float -> World -> Picture
drawMenu horizontalResolution verticalResolution world
    = translate left top $ scaleBoth (worldScale) $ color white $
             (translate 10 (textBaseH * (-0.5) - 10) $
                  scaleBoth 0.5 $ text "Hasteroids")
          <> drawOptions
    where worldScale      = verticalResolution / 576
          top             = verticalResolution / 2
          left            = horizontalResolution / (-2)
          textBaseH       = 110
          (drawOptions,_)
              = foldl (\(picture, i) option ->
                            (picture
                             <> (translate 10
                                           (-25
                                            - textBaseH * (0.8 + 0.3 * (fromIntegral i))
                                            - 15 * (fromIntegral i))
                                            $ scaleBoth 0.3
                                            $ text $ optionText option i),
                             i + 1))
                      (blank, 0)
                      menuOptions
              where optionText :: String -> Int -> String
                    optionText option num | num == world^.menu.selectionOption = '>' : option
                                          | otherwise                          = option