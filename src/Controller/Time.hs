{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp, FlexibleContexts                              #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))
import Control.Lens
import Control.Monad
import Control.Monad.State

import Data.List

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model

-- | Time handling

--This is where we will change the gameworld (Update)
--time is the passed time in seconds (gameTime)
timeHandler :: Float -> World -> World
timeHandler time = execState changeWorld

--Functions needed for using states
--Important types:
--StateT    :: (s -> m(a,s))  -> StateT s a
--State     :: StateT s Identity
--(State    :: (s -> (a,s))   -> State s a)
--runState  :: State s a -> s -> (a, s) 
--execState :: State s a -> s -> s
--(In runState, the s given is a starting state, in our case the current world)
--(Identity is a monad, that returns the normal value, runIdentity :: a)

--Change the world in the MonadState, then get the final world and return it.
changeWorld :: MonadState World m => m World
changeWorld = do player.playerPos.x += 1
                 player.playerPos.y -= 1
                 moveEnemies
                 get

moveEnemies :: MonadState World m => m World
moveEnemies = do ppos <- use $ player.playerPos
                 enemies.traversed.enemyPos %= moveTo 2 ppos
                 get

-- Move a certain amount of pixels to a goal.
moveTo :: Float -> Point -> Point -> Point
moveTo speed goal start = Point {_x = start^.x + speed * sin dir, _y = start^.y + speed * cos dir}
                        where dir = pointDirection start goal

-- Get the direction between two points
pointDirection :: Point -> Point -> Float
pointDirection p1 p2 = atan2 (p2^.x - p1^.x) (p2^.y - p1^.y)



