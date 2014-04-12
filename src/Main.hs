-- ----------------------------------------------------------------------------
-- Main.hs for hexif project
-- ----------------------------------------------------------------------------

import DataExif
import Reader
import PrettyPrint

import qualified Data.ByteString.Lazy as BL

main :: IO()
main = do
   inp <- BL.readFile "JG1111.exif"
   let input = BL.drop 6 inp
   let ifdDir = readExif input 
   mapM_ print (prettyPrint ifdDir)

