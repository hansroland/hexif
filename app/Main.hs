module Main where

import Graphics.Hexif
import Graphics.Hexif.Api
import System.Environment ( getArgs )

main :: IO ()
main = do
    args <- getArgs
    if null args
      then putStrLn "usage: hexif filename"
      else do
        exif <- fromFile $ head args
        mapM_ putStrLn $ map format $ prettyPrint exif
        putStrLn ""
        putStrLn "selected Fields"
        print $ getTag exif TagGPSLatitude
        print $ getDateTime exif
        putStrLn "End of Processing"

format :: PrettyEntry -> String
format (PrettyTitle t) = "\n" ++ t
format (PrettyTag t v) = "  " ++ t ++ " -> " ++ v
