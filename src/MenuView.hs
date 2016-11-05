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
                  scaleBoth 0.5 $ text getTitle)
          <> (translate 10 (textBaseH * (-0.7) - 20) $
                  scaleBoth 0.2 $ text getSubTitle)
          <> drawOptions
    where worldScale      = verticalResolution / 576
          top             = verticalResolution / 2
          left            = horizontalResolution / (-2)
          textBaseH       = 110
          (drawOptions,_)
              = foldl (\(picture, i) option ->
                            (picture
                             <> (translate 10
                                           (-35
                                            - textBaseH * (1 + 0.3 * (fromIntegral i))
                                            - 15 * (fromIntegral i))
                                            $ scaleBoth 0.3
                                            $ text $ optionText option i),
                             i + 1))
                      (blank, 0) $
                      menuOptions isDieMenu
              where optionText :: String -> Int -> String
                    optionText option num | num == world^.menu.selectionOption = '>' : option
                                          | otherwise                          = option
          isDieMenu   = world^.menu.hasDiedBefore
          getTitle    = if isDieMenu then
                            "You died!"
                        else
                            "Hasteroids"
          getSubTitle = if isDieMenu then
                            if world^.isNewHighscore then
                                scoreText ++ "new highscore!"
                            else
                                scoreText ++ "highscore: " ++ (show $ world^.highscore)
                        else
                            "By Martin Boers and Florian van Strien"
                        where scoreText = "Score: " ++ (show $ world^.player.score) ++ "; "