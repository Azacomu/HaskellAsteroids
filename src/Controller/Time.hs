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
import Data.Maybe

import Graphics.Gloss hiding (Point)
import Graphics.Gloss.Geometry.Angle

import System.Random

import Helper
import Model

import Controller.MenuUpdate

-- | Time handling

--This is where we will change the gameworld (Update)
--time is the passed time in seconds (gameTime)
timeHandler :: Float -> World -> IO World
timeHandler time world = return $ execState (changeWorld time) world

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
                          updateBullets
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
                    checkPlayer
                    
--Checks whether the player is still inside the screen and is not dead                    
checkPlayer :: MonadState World m => m ()
checkPlayer = do pos <- use $ player.playerPos
                 case outsideBounds pos 20 of
                    East  -> player.playerPos.x -= screenWidth
                    West  -> player.playerPos.x += screenWidth
                    North -> player.playerPos.y -= screenHeight
                    South -> player.playerPos.y += screenHeight
                    None  -> return ()

--Changes the world for when the player dies (TODO)
diePlayer :: MonadState World m => m ()
diePlayer = return ()

--Checks whether given point is outside the screen (with given offset to each side)                  
outsideBounds :: Point -> Float -> Side
outsideBounds p offset | p^.x - offset > 0.5 * screenWidth   = East
                       | p^.x + offset < -0.5 * screenWidth  = West
                       | p^.y - offset > 0.5 * screenHeight  = North
                       | p^.y + offset < -0.5 * screenHeight = South
                       | otherwise                           = None
 
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
                    
--Checks whether bullets collide and updates the lifetime and deletes the bullet if it times out
updateBullets :: MonadState World m => m ()
updateBullets = do es <- use enemies
                   bs <- use bullets
                   let col       = unzip $ mapMaybe (collideWith es) bs
                   let infst x   = x `elem` (fst col)
                   let insnd x   = x `elem` (snd col)
                   let timeout b = b^.bulTime <= 0
                   sMul         <- use $ player.scoreMul
                   player.score += length (fst col) * sMul
                   bullets      .= filter (\b -> not (infst b || timeout b)) bs
                   enemies      .= filter (not . insnd) es
                   bullets.traversed.bulTime -= 1
                   

--Checks if there is a collision and returns it, only returns one collision, as one bullet can only collide with one enemy
collideWith :: [Enemy] -> Bullet -> Maybe (Bullet, Enemy)
collideWith enemies b | filtered == [] = Nothing
                      | otherwise      = Just (b, (head filtered))
                      where filtered = filter (\e -> pointDistance (e^.enemyPos) (b^.bulPos) < 20) enemies
          

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