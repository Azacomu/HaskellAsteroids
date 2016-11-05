module MenuView where

import Graphics.Gloss hiding (Point)
import Data.Monoid

import Control.Lens
import Helper
import Controller.MenuUpdate

import Model

drawMenu :: Float -> Float -> World -> Picture
drawMenu horizontalResolution verticalResolution world
    =        translate left top $ scaleBoth worldScale $ color white $
             translate 10 (textBaseH * (-0.5) - 10) (scaleBoth 0.5 $ text getTitle)
          <> translate 10 (textBaseH * (-0.7) - 20) (scaleBoth 0.2 $ text getSubTitle)
          <> drawOptions
    where worldScale      = verticalResolution / 576
          top             = verticalResolution / 2
          left            = horizontalResolution / (-2)
          textBaseH       = 110
          (drawOptions,_) = foldl (drawMenuOption world) (blank, 0) $ menuOptions isDieMenu
          isDieMenu       = world^.menu.hasDiedBefore
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
                        
drawMenuOption :: World -> (Picture, Int) -> String -> (Picture, Int)
drawMenuOption world (picture, i) option = (picture <> newPicture, i + 1)
                        where textBaseH  = 110
                              selected   = world^.menu.selectionOption
                              newPicture = translate 10 transY (scaleBoth 0.3 $ text $ optionText option i)
                              transY     = -35 - textBaseH * (1 + 0.3 * fromIntegral i) - 15 * fromIntegral i 
                              optionText :: String -> Int -> String
                              optionText opt num | num == selected = '>' : opt
                                                 | otherwise       =       opt