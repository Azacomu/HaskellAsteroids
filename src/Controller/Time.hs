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

--Change the world in the MonadState
changeWorld :: MonadState World m => m ()
changeWorld = do player.playerPos.x += 1
                 player.playerPos.y -= 1
                 spawnEnemies
                 moveEnemies

-- Spawn new enemies every now and then
spawnEnemies :: MonadState World m => m ()
spawnEnemies = do spawner <- use enemySpawner
                  enemySpawner.timeToNext -= 1
                  when (spawner^.timeToNext <= 0) $ do
                      playerPos <- use $ player.playerPos
                      spawnPos <- getRandomSpawnPoint
                      enemies %= (newEnemy spawnPos (pointDirection spawnPos playerPos) :)
                      enemySpawner.timeToNext += spawner^.interval

-- Move the enemies in the world
moveEnemies :: MonadState World m => m ()
moveEnemies = do playerPos <- use $ player.playerPos
                 enemies.traversed %= moveEnemy playerPos

-- Move a single enemy (needs the player position)
moveEnemy :: Point -> Enemy -> Enemy
moveEnemy playerPos e
    = set enemyPos
          (if e^.movementType == FixedDirection then
               moveDir (e^.enemyDir) 5 (e^.enemyPos)
           else
               moveTo 5 playerPos (e^.enemyPos))
          e

-- Move a certain amount of pixels to a goal.
moveTo :: Float -> Point -> Point -> Point
moveTo speed goal start = moveDir (pointDirection start goal) speed start

-- Move a certain amount of pixels in a given direction
moveDir :: Float -> Float -> Point -> Point
moveDir dir speed start = Point { _x = start^.x + speed * sin dir, _y = start^.y + speed * cos dir }

-- Get the distance between two points
pointDistance :: Point -> Point -> Float
pointDistance p1 p2 = sqrt $ (p2^.x - p1^.x) ** 2 + (p2^.y - p1^.y) ** 2

-- Get the direction between two points
pointDirection :: Point -> Point -> Float
pointDirection p1 p2 = atan2 (p2^.x - p1^.x) (p2^.y - p1^.y)

-- Get a random point at a certain minimum distance from the player
getRandomSpawnPoint :: MonadState World m => m (Point)
getRandomSpawnPoint = do pPos   <- use $ player.playerPos
                         spawnX <- getRandomR (-400, 400)
                         spawnY <- getRandomR (-300, 300)
                         let spawnPos = Point {_x = spawnX, _y = spawnY}
                         if pointDistance spawnPos pPos > 250 then
                             return spawnPos
                         else
                             getRandomSpawnPoint

-- Get a random value using the world state
getRandomR :: (MonadState World m, Random a) => (a, a) -> m (a)
getRandomR range = do generator <- use rndGen
                      let r = randomR range generator
                      rndGen .= snd r
                      return $ fst r