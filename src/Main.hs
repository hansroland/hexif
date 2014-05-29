-- ----------------------------------------------------------------------------
-- Main.hs for hexif project
-- ----------------------------------------------------------------------------
-- 
-- Here we show all tags including the debug tags
--
-- ----------------------------------------------------------------------------

import Graphics.Hexif
import System.IO
import System.Environment (getArgs)
import System.FilePath

-- | main entry of the program
main :: IO()
main = do
     args <- getArgs
     if null args
       then putStrLn "usage: ???? filename"
       else processFile $ head args

-- | Pretty print all tags
processFile :: FilePath -> IO()
processFile fn = do
    exif <- fromFile fn
    mapM_ print (allFieldsInclDebug exif)

-- | Pretty print a single tag
singleTag :: FilePath -> ExifTag -> IO()
singleTag fn tag = do
    exif <- fromFile fn
    print $ getTag exif tag


