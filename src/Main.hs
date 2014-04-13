-- ----------------------------------------------------------------------------
-- Main.hs for hexif project
-- ----------------------------------------------------------------------------

import Hexif

main :: IO()
main = do
   exif <- fromFile "JG1111.exif"
   mapM_ print (allTags exif)

