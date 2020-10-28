module Main where

import Graphics.Hexif
import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    if null args
      then putStrLn "usage: hexif filename"
      else do
        exif <- fromFile $ head args
        mapM_ print $ allFields exif
        putStrLn ""
        putStrLn "selected Fields"
        print $ getTag exif TagGPSLatitude
        putStrLn "End of Processing"
