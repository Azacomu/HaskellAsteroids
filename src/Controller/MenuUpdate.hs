{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, FlexibleContexts #-}

module Controller.MenuUpdate where

import Control.Lens
import Control.Monad
import Control.Monad.State
import System.Exit

import Model

--Menu options for the die menu or the normal one
--(True if just died)
menuOptions :: Bool -> [String]
menuOptions True  = ["Play Again", "Quit"]
menuOptions False = ["Play", "Quit"]

--Update the menu using the world state
updateMenu :: StateT World IO ()
updateMenu = do hasJustDied <- use $ menu.hasDiedBefore --Whether we just died
                let menuOptionNum = length $ menuOptions hasJustDied
                --Go to the previous menu option if selected by the player
                selectsPrev <- use doesSelectPrev
                when selectsPrev $
                    menu.selectionOption %= max 0 . subtract 1
                --Go the the next menu option if selected
                selectsNext <- use doesSelectNext
                when selectsNext $
                    menu.selectionOption %= min (menuOptionNum - 1) . (+) 1
                --Do something when we select a menu option
                confirms    <- use doesConfirm
                nowSelected <- use $ menu.selectionOption
                when confirms $
                    case nowSelected of --Play
                                        0 -> do player.score .= 0
                                                gameState    .= InGame
                                        --Quit
                                        1 -> lift exitSuccess