module Highscores where

import System.Directory



--Const for the filepath
filePath :: FilePath
filePath = "game.highscore"


--Returns the saved highscore (and 0 if there is no highscore yet)
getHighscore :: IO Int
getHighscore = do exists <- doesFileExist filePath
                  if not exists then return 0
                  else do
                         s <- readFile filePath
                         return $ read s
                  

--Saves highscore to file
saveHighscore :: Int -> IO ()
saveHighscore x = writeFile filePath (show x)

--Checks whether given int is a highscore or not
checkHighscore :: Int -> IO Bool
checkHighscore x = do hs <- getHighscore
                      return $ if hs >= x then False else True