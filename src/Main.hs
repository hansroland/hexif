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
   exif <- fromFile "JG1111.jpg"
   -- exif <- fromFile "RS4847.JPG"
   mapM_ print (allTagsInclDebug exif)



