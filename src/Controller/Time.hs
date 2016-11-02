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

import Helper
import Model

import Controller.MenuUpdate

-- | Time handling

--This is where we will change the gameworld (Update)
--time is the passed time in seconds (gameTime)
timeHandler :: Float -> World -> World
timeHandler time = execState (changeWorld time)

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
changeWorld :: MonadState World m => Float -> m ()
changeWorld time = do curState <- use gameState
                      if curState == InMenu then
                          updateMenu
                      else do
                          tickTime   .= time
                          passedTime += time
                          rotatePlayer
                          movePlayer
                          shootPlayer
                          moveBullets
                          spawnBonuses
                          pickupBonuses
                          spawnEnemies
                          moveEnemies
                      resetKeys

--Reset some keys that should only be handled on press
resetKeys :: MonadState World m => m()
resetKeys = do doesConfirm    .= False
               doesSelectPrev .= False
               doesSelectNext .= False

--Move the player if needed
movePlayer :: MonadState World m => m ()
movePlayer = do moveAction <- use movementAction
                when (moveAction == Thrust) $ do
                    p <- use player
                    player.playerPos .= moveDir (p^.playerDir) (p^.playerSpeed) (p^.playerPos)
 
--Rotate the player if needed 
rotatePlayer :: MonadState World m => m ()
rotatePlayer = do rAction <- use rotateAction
                  speed   <- use $ player.playerSpeed
                  case rAction of
                    RotateLeft  -> player.playerDir -= (speed / 180) * pi
                    RotateRight -> player.playerDir += (speed / 180) * pi 
                    NoRotation  -> return ()

--Shoots if the player wants to shoot and the time since the last shot is long enough                    
shootPlayer :: MonadState World m => m ()
shootPlayer = do p     <- use player
                 shoot <- use shootAction
                 player.shootTime -= 1
                 when (shoot == Shoot && p^.shootTime <= 0) $ do
                    bullets          %= (newBullet (p^.playerPos) (p^.playerDir) :)
                    player.shootTime .= p^.baseShootTime
                    
--Moves all bullets
moveBullets :: MonadState World m => m ()
moveBullets = bullets.traversed %= moveBullet

--Moves a bullet
moveBullet :: Bullet -> Bullet
moveBullet b = b & bulPos .~ moveDir (b^.bulDir) (b^.bulSpeed) (b^.bulPos)

-- Spawn new bonuses now and then
spawnBonuses :: MonadState World m => m ()
spawnBonuses = do spawner <- use bonusSpawner
                  bonusSpawner.timeToNext -= 1
                  when (spawner^.timeToNext <= 0) $ do
                      playerPos <- use $ player.playerPos
                      spawnPos <- getRandomSpawnPoint
                      bonuses %= (newBonus spawnPos :)
                      bonusSpawner.timeToNext += spawner^.interval

-- Have the player pick up bonuses
pickupBonuses :: MonadState World m => m ()
pickupBonuses = do playerPos <- use $ player.playerPos
                   currentBonuses <- use bonuses
                   let collidingBonuses = filter (\b -> pointDistance playerPos (b^.bonusPos) < 40) currentBonuses
                   when (not $ null collidingBonuses) $ do
                       player.scoreMul += 1
                       bonuses %= filter (not . (`elem` collidingBonuses)) -- Destroy any colliding enemies

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
                 -- Check if any enemies collide with the player
                 currentEnemies <- use enemies
                 let collidingEnemies = filter (\e -> pointDistance playerPos (e^.enemyPos) < 40) currentEnemies
                 when (not $ null collidingEnemies) $ do
                     player.scoreMul .= 1
                     enemies %= filter (not . (`elem` collidingEnemies)) -- Destroy any colliding enemies

-- Move a single enemy (needs the player position for tracking enemies)
moveEnemy :: Point -> Enemy -> Enemy
moveEnemy playerPos e
    = e & enemyPos .~ if e^.movementType == FixedDirection then
                          moveDir (e^.enemyDir) 5 (e^.enemyPos)
                      else
                          moveTo 5 playerPos $ e^.enemyPos

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