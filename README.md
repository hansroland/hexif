## Read the EXIF File of a JPEG image with Haskell Code only.

This hexif library has similar functionality as the *exif* package [http://hackage.haskell.org/package/exif-3000.0.0/docs/Graphics-Exif.html](http://hackage.haskell.org/package/exif-3000.0.0/docs/Graphics-Exif.html). The exif package contains the bindings (wrapper) to the libexif C EXIF library [http://libexif.sourceforge.net/](http://libexif.sourceforge.net/).

The first example shows how to print out all supported exif information of a JPEG image.

    processFile :: FilePath -> IO()
    processFile fn = do
        exif <- fromFile fn
        mapM_ print (allTags exif)   
    -- processFile "RS4748.JPG"

The next example prints out the value of a single tag:

    singleTag :: FilePath -> ExifTag -> IO()
    singleTag fn tag = do
        exif <- fromFile fn
        print $ getTag exif tag 
    -- singleTag "RS4847.JPG" TagComponentsConfiguration


