-- ----------------------------------------------------------------------------
-- Main.hs for hexif project
-- ----------------------------------------------------------------------------
-- 
-- Here we show all tags including the debug tags
--
-- ----------------------------------------------------------------------------

import Graphics.Hexif
import System.IO
import System.Environment
import System.FilePath


main :: IO()
main = do
     args <- getArgs
     if length args == 0
       then putStrLn "usage: ???? filename"
       else processFile $ head args

processFile :: FilePath -> IO()
processFile fn = do
   exif <- fromFile fn
   mapM_ print (allTagsInclDebug exif)
