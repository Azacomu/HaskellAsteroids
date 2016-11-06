{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, FlexibleContexts #-}

module Controller.Time (timeHandler) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Graphics.Gloss hiding (Point)
import System.Random

import Helper
import Highscores
import Model

import Controller.MenuUpdate

-- | Time handling

--This is where we will change the gameworld (Update)
--time is the passed time in seconds (gameTime)
timeHandler :: Float -> World -> IO World
timeHandler time world | world^.endTimer > 0 = 
                         do let nWorld = execState reduceEndTimer world
                            if  nWorld^.endTimer <= 0 then
                                execStateT diePlayer nWorld
                            else
                                execStateT (changeWorld time) nWorld
                        | world^.player^.lives <= 0 =
                          return $ execState setEndTimer world
                        | world^.isHighSet = 
                          execStateT (changeWorld time) world
                        | otherwise = 
                          do hsWorld <- execStateT setWorldHighscore world
                             execStateT (changeWorld time) hsWorld
                                 
--End of the world: a short time where the player is dead and we 
--haven't returned to the main menu yet
                 
--Set the final timer; this should happen when the player loses their last
--life     
setEndTimer :: MonadState World m => m ()
setEndTimer = endTimer .= 20

--Reduce the end timer; this should happen each Update after the player has
--lost their last life
reduceEndTimer :: MonadState World m => m ()
reduceEndTimer = endTimer -= 1

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
changeWorld :: Float -> StateT World IO ()
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
                          spawnBonuses
                          pickupBonuses
                          spawnEnemies
                          moveEnemies
                          updateParticles
                      resetKeys
                      handleStars

--Sets the highscore var in the world
setWorldHighscore :: StateT World IO ()
setWorldHighscore = do hs <- lift getHighscore
                       highscore .= hs
                       isHighSet .= True

--Changes the world for when the player dies
diePlayer :: StateT World IO ()
diePlayer = do scoreT <- use $ player.score
               better <- lift $ checkHighscore scoreT
               newSeed <- getRandomR (0, 100000)
               put $ initial newSeed --Reset state
               when better $ do lift $ saveHighscore scoreT
                                highscore      .= scoreT
                                isNewHighscore .= True
               menu.hasDiedBefore .= True
               player.score       .= scoreT --Set score again
                      
--Reset some keys that should only be handled on press
resetKeys :: MonadState World m => m()
resetKeys = do doesConfirm    .= False
               doesSelectPrev .= False
               doesSelectNext .= False

--Move the player if needed (and still possible)
--Also spawn particles in the direction the player is moving
movePlayer :: MonadState World m => m ()
movePlayer = do moveAction <- use movementAction
                lvs        <- use $ player.lives
                p          <- use player
                let plrPos  = player.playerPos
                when (moveAction == Thrust && lvs > 0) $ do
                    let newDir    = moveDir (p^.playerDir) (p^.playerSpeed) (p^.playerPos)
                    particles    %= (newParticle (p^.playerPos) 10 yellow :)
                    plrPos       .= checkPosition newDir (p^.playerSize)
                when (moveAction == BackThrust && lvs > 0) $ do
                    let newDir    = moveDir (p^.playerDir) (p^.playerSpeed / (-2)) (p^.playerPos)
                    let partPos1  = moveDir (p^.playerDir - (0.5 * pi)) 8 (p^.playerPos)
                    let partPos2  = moveDir (p^.playerDir + (0.5 * pi)) 8 (p^.playerPos)
                    particles    %= (newParticle partPos1 5 yellow :)
                    particles    %= (newParticle partPos2 5 yellow :)
                    plrPos       .= checkPosition newDir (p^.playerSize)
                    
--Checks whether the position with given offset is still inside the screen
--if not returns the new position                  
checkPosition :: Point -> Float -> Point
checkPosition pos off = case outsideBounds pos off of
                            East  -> (x -~ screenWidth  + off) pos
                            West  -> (x +~ screenWidth  + off) pos
                            North -> (y -~ screenHeight + off) pos
                            South -> (y +~ screenHeight + off) pos
                            None  -> pos

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
shootPlayer = do p                <- use player
                 shoot            <- use shootAction
                 player.shootTime -= 1
                 when (shoot == Shoot && p^.shootTime <= 0 && p^.invincibleTime <= 0) $
                  do bullets          %= (newBullet (p^.playerPos) (p^.playerDir) :)
                     player.shootTime .= p^.baseShootTime
                    
--Moves all bullets
moveBullets :: MonadState World m => m ()
moveBullets = allBullets %= moveBullet

--Moves a bullet
moveBullet :: Bullet -> Bullet
moveBullet b = b & bulPos .~ moveDir (b^.bulDir) (b^.bulSpeed) (b^.bulPos)

--Spawn new bonuses now and then
--Uses a bonus spawner to keep track of the time until the next bonus appears
spawnBonuses :: MonadState World m => m ()
spawnBonuses = do spawner      <- use bonusSpawner
                  let timeNextB = bonusSpawner.timeToNext
                  timeNextB    -= 1
                  when (spawner^.timeToNext <= 0) $ do
                      --Actually spawn a bonus!
                      spawnPos  <- getRandomSpawnPoint
                      bonuses   %= (newBonus spawnPos :)
                      --And reset the timer
                      timeNextB += spawner^.interval

--Have the player pick up bonuses
--And destroy those bonusses afterwards (so they can't be picked up multiple times)
pickupBonuses :: MonadState World m => m ()
pickupBonuses = do plrPos              <- use $ player.playerPos
                   plrSize             <- use $ player.playerSize
                   currentBonuses      <- use bonuses
                   --Check which bonuses the player finds
                   let plrPickup b      = pointDistance plrPos (b^.bonusPos) < bonusSize + plrSize
                   let collidingBonuses = filter plrPickup currentBonuses
                   unless (null collidingBonuses) $ do
                       --Destroy the bonuses and add to the score multiplier
                       player.scoreMul += length collidingBonuses
                       bonuses         %= filter (not . (`elem` collidingBonuses))

--Checks whether bullets collide and updates the lifetime and deletes the bullet if it times out
updateBullets :: MonadState World m => m ()
updateBullets = do es <- use enemies
                   bs <- use bullets
                   bn <- use bonuses
                   let colEnemy  = unzip $ mapMaybe (collideWith es) bs
                   let colBonus  = unzip $ mapMaybe (collideWith bn) bs
                   let infst c n = n `elem` fst c
                   let insnd c n = n `elem` snd c
                   let timeout b = b^.bulTime <= 0
                   let bFilter b = not (infst colEnemy b || infst colBonus b || timeout b)
                   player.scoreMul    += length (snd colBonus)
                   sMul               <- use $ player.scoreMul
                   player.score       += length (fst colEnemy) * sMul
                   bullets            .= filter bFilter bs
                   explodeEnemies      $ snd colEnemy
                   es2                <- use enemies --We could have spawned new enemies
                   enemies            .= filter (not . insnd colEnemy) es2
                   bonuses            .= filter (not . insnd colBonus) bn
                   allBullets.bulTime -= 1                

--Let enemies in the monad explode
explodeEnemies :: MonadState World m => [Enemy] -> m ()
explodeEnemies []           = return ()
explodeEnemies (thisE:allE)
    = do explodeEnemies allE --Let the other enemies explode
         --Add particles
         let addPart p =  newParticle (p `addPoints` (thisE^.enemyPos)) 10 red
         particles    %= (map addPart (thisE^.enemyEdges) ++)
         --Spawn new enemies if the enemy was very big
         when (thisE^.enemySize > 30) $ do
             startingAngle <- getRandomR (0, pi / 2)
             spawnE startingAngle
             spawnE $ startingAngle + pi / 2
             spawnE $ startingAngle + pi
             spawnE $ startingAngle + pi * 1.5
    where
        spawnE angle --Spawn an enemy at a given angle from the old enemy
         = do segmentNum <- getRandomR (5 :: Int, 10)
              generator  <- use rndGen
              let newSize = thisE^.enemySize / 2
              let edgePoints = getEnemyPoints newSize segmentNum generator
              enemies %= (newEnemy (moveDir angle newSize (thisE^.enemyPos))
                                   angle
                                   edgePoints
                                   newSize
                                   (thisE^.enemySpeed) :)

--Spawn new enemies every now and then
spawnEnemies :: MonadState World m => m ()
spawnEnemies
    = do spawner      <- use enemySpawner
         let timeNextE = enemySpawner.timeToNext
         timeNextE    -= 1
         when (spawner^.timeToNext <= 0) $ do
             --We need to spawn a new enemy!
             plrPos         <- use $ player.playerPos
             spawnPos       <- getRandomSpawnPoint
             isFollowing    <- getRandom
             timeNextE      += spawner^.interval
             --Check what type of enemy we should spawn
             if isFollowing < followingChance then
                 --Spawn an enemy which follows the player
                 enemies    %= (newFollowingEnemy spawnPos getFollowingEnemyPoints 16 :)
             else do
                 --Spawn a asteroid
                 thisSize   <- getRandomR (15, 45)
                 segmentNum <- getRandomR (5, 15)
                 spd        <- getRandomR (3, 5)
                 generator  <- use rndGen
                 --Create a random sprite
                 let eps     = getEnemyPoints thisSize segmentNum generator
                 let dir     = pointDirection spawnPos plrPos
                 enemies    %= (newEnemy spawnPos dir eps thisSize spd :)
                      

--Get points forming a following enemy
getFollowingEnemyPoints :: [Point]
getFollowingEnemyPoints = [ Point {_x = 0  , _y = 0  }
                          , Point {_x = -16, _y = -16}
                          , Point {_x = 0  , _y = -8 }
                          , Point {_x = 16 , _y = -16}
                          , Point {_x = 0  , _y = 0  }
                          , Point {_x = -16, _y = 0  }
                          , Point {_x = 0  , _y = 16 }
                          , Point {_x = 16 , _y = 0  }
                          ]

--Get points forming an asteroid-like shape, given the size of this shape, the
--number of points and a random generator
--This works by making a circle-like shape with some random deviations of the
--points
getEnemyPoints :: RandomGen g => Float -> Int -> g -> [Point]
getEnemyPoints size num g
    = helper num g
    where --The helper creates a single point, given the index of that point
          --and a random generator.
          helper 0 _   = []
          helper i gen = moveDir direction (size * val) (newPoint 0 0) : helper (i - 1) newGen
                       where --Get a random deviation from the circle between
                             --1 and 1.3.
                             (val, newGen) = randomR (1, 1.3) gen
                             --Get the direction of the current point from
                             --the center point of the circle
                             direction     = fromIntegral i / fromIntegral num * 2 * pi
                               

--Move the enemies in the world, Collide them with the player
--Destroy the colliding enemies and create explosion particles
moveEnemies :: MonadState World m => m ()
moveEnemies = do plrPos     <- use $ player.playerPos
                 plrSize    <- use $ player.playerSize
                 invcT      <- use $ player.invincibleTime
                 allEnemies %= moveEnemy plrPos
                 --Check if any enemies collide with the player (if the player isn't invincible)
                 if invcT > 0 then
                     player.invincibleTime -= 1
                 else do
                     currentEnemies      <- use enemies
                     let getDist e        = pointDistance plrPos (e^.enemyPos)
                     let plrHit e         = getDist e < (e^.enemySize) + plrSize
                     let collidingEnemies = filter plrHit currentEnemies
                     --Collide with player. Reset the score multiplier, remove
                     --on life, make the player invincible for a while, remove
                     --the colliding enemy and make it explode.
                     unless (null collidingEnemies) $ do
                         player.scoreMul       .= 1
                         player.lives          -= 1
                         player.invincibleTime += invincibleTimeAfterCollision
                         enemies               %= filter (not . (`elem` collidingEnemies))
                         explodeEnemies collidingEnemies
                         --Spawn explosion particles for the player
                         replicateM_ 100 $ do
                             dir            <- getRandomR (0, 2 * pi)
                             dist           <- getRandomR (0, plrSize)
                             let particlePos = moveDir dir dist plrPos
                             particles      %= (newParticle particlePos 10 white :)

--Move a single enemy (needs the player position for tracking enemies)
moveEnemy :: Point -> Enemy -> Enemy
moveEnemy plrPos e
    = e & enemyPos .~ 
      if e^.movementType == FixedDirection then
          --Move the enemy in the original direction
          checkPosition (moveDir (e^.enemyDir) (e^.enemySpeed) (e^.enemyPos)) (e^.enemySize)
      else
          --Move towards the player
          moveTo (e^.enemySpeed) plrPos $ e^.enemyPos

--Move stars and spawn new ones
handleStars :: MonadState World m => m ()
handleStars = do --Move all stars
                 stars.traversed %= (\star -> star & starPos . x -~ (star^.starSpeed))
                 --Remove stars that get outside of the screen
                 stars           %= filter (\star -> star^.starPos.x > -screenWidth / 2)
                 shouldSpawnStar <- getRandom
                 when (shouldSpawnStar < starSpawnChance) $ do
                     --Spawn a new star!
                     newStarPos <- getRandomR (-screenHeight / 2, screenHeight / 2)
                     thisSpeed  <- getRandomR (1, 6)
                     stars      %= (newStar (newPoint (screenWidth / 2) newStarPos) thisSpeed :)

--Make the particles smaller
updateParticles :: MonadState World m => m ()
updateParticles = do allParticles.partSize -= 1
                     particles             %= filter (\p -> p^.partSize > 0)

--Get a random point at a certain minimum distance from the player
getRandomSpawnPoint :: MonadState World m => m Point
getRandomSpawnPoint = do pPos   <- use $ player.playerPos
                         spawnX <- getRandomR (-screenWidth / 2, screenWidth / 2)
                         spawnY <- getRandomR (-screenHeight / 2, screenHeight / 2)
                         let spawnPos = newPoint spawnX spawnY
                         --Check if the calculated point is far enough from
                         --the player
                         if  pointDistance spawnPos pPos > 300 then
                             return spawnPos     --Yes, it is! Return it.
                         else
                             getRandomSpawnPoint --No, get a new one.

--Get a random value in a range using the world state
getRandomR :: (MonadState World m, Random a) => (a, a) -> m a
getRandomR range = do generator <- use rndGen
                      let (r, g) = randomR range generator
                      rndGen    .= g
                      return r

--Get a random value using the world state (in the default range)
getRandom :: (MonadState World m, Random a) => m a
getRandom = do generator <- use rndGen
               let (r, g) = random generator
               rndGen    .= g
               return r