-- ----------------------------------------------------------------------------
-- Main.hs for hexif project
-- ----------------------------------------------------------------------------
-- 
-- Here we show all tags including the debug tags
--
-- ----------------------------------------------------------------------------

import Graphics.Hexif
import System.IO

main :: IO()
main = do
   -- exif <- fromFile "JG1111.exif"
   exif <- fromFile "RS4847.exif"
   mapM_ print (allTagsInclDebug exif)



