-- ----------------------------------------------------------------------------
-- Main.hs for hexif project
-- ----------------------------------------------------------------------------

import Graphics.Hexif

main :: IO()
main = do
   -- exif <- fromFile "JG1111.exif"
   exif <- fromFile "RS4847.exif"
   mapM_ print (allTags exif)

