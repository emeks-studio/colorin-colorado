
module ColorinColorado.Types.Painter
  ( Painter,
    coloringFile,
    AnyPalette (AnyPalette),
    RGBPainter (RGBPainter),
    RGBAPainter (RGBAPainter),
  ) where

import Conduit
  ( mapC,
    runConduitRes,
    sinkList,
    sourceFileBS,
    (.|),
    MonadUnliftIO, 
  )
import ColorinColorado.Types.Colors (HexColor, parseHexColorFromRGB, parseHexColorFromRGBA)
import ColorinColorado.Types.Palette (Palette, colorBSWith)
import Data.Conduit.Combinators (chunksOfE)
import Data.ByteString (ByteString)
import Control.Monad (join)
import Data.Either.Extra (eitherToMaybe)

class Painter a where
  coloringFile :: (MonadUnliftIO m) => a -> FilePath -> m (Maybe [HexColor])

newtype AnyPalette p = AnyPalette p 

instance Palette p => Painter (AnyPalette p) where
  coloringFile (AnyPalette p) sourceFilePath = do
    let color = colorBSWith p
    chunks <-
      runConduitRes $
        sourceFileBS sourceFilePath .| mapC color .| sinkList
    let mColoredChunks = sequence chunks
    let mColoredFile = join <$> mColoredChunks
    return mColoredFile

data RGBPainter = RGBPainter

instance Painter RGBPainter where
  coloringFile _ sourceFilePath = do
    let color :: ByteString -> Maybe HexColor
        color = eitherToMaybe . parseHexColorFromRGB
    chunks <-
      runConduitRes $
        sourceFileBS sourceFilePath .| chunksOfE 3 .| mapC color .| sinkList
    let mColoredFile = sequence chunks
    return mColoredFile

data RGBAPainter = RGBAPainter

instance Painter RGBAPainter where
  coloringFile _ sourceFilePath = do
    let color :: ByteString -> Maybe HexColor
        color = eitherToMaybe . parseHexColorFromRGBA
    chunks <-
      runConduitRes $
        sourceFileBS sourceFilePath .| chunksOfE 4 .| mapC color .| sinkList
    let mColoredFile = sequence chunks
    return mColoredFile
