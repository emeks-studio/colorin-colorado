{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.Types.Palette
  ( Palette, -- General purpose interface
    lookupHexColor,
    lookupByte,
    SimplePalette256, -- Specific implementation
    mkSimplePalette256, -- Smart constructor for SimplePalette256
    colorFileWith,
    colorFileWith',
    colorFileWith''
  )
where

import ColorinColorado.Types.Colors (HexColor, parseHexColorFromRGB, parseHexColorFromRGBA)
import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Array),
  )
import qualified Data.Bimap as Bimap
  ( Bimap,
    fromList,
    toList,
    lookup,
    lookupR,
  )
import Data.List as List (length)
import Data.List.Unique as List.Unique (allUnique)
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Data.Foldable (toList)
import Conduit
  ( mapC,
    runConduitRes,
    sinkList,
    sourceFileBS,
    (.|),
    MonadUnliftIO, 
  )
import Data.Conduit.Combinators (chunksOfE)
import Data.ByteString (ByteString, unpack)
import Control.Monad (join)
import Data.Either.Extra (eitherToMaybe)

class Palette a where
  lookupHexColor :: a -> Word8 -> Maybe HexColor
  lookupByte :: a -> HexColor -> Maybe Word8

newtype SimplePalette256 = SimplePalette256 (Bimap.Bimap Word8 HexColor)
  deriving stock (Generic, Eq, Ord, Show)

instance Palette SimplePalette256 where
  lookupHexColor (SimplePalette256 palette) byte = Bimap.lookup byte palette
  lookupByte (SimplePalette256 palette) color = Bimap.lookupR color palette

mkSimplePalette256 :: [HexColor] -> Either String SimplePalette256
mkSimplePalette256 colors = do
  _ <- has256Elements colors "Palette must have 256 colors"
  _ <- notRepeatedElements colors "Palette must have all unique colors"
  let tupleList = zip possibleBytes colors
  return $ SimplePalette256 $ Bimap.fromList tupleList

possibleBytes :: [Word8]
possibleBytes = [0 .. 255]

has256Elements :: [a] -> String -> Either String ()
has256Elements list errorMessage =
  if List.length list == 256 then Right () else Left errorMessage

notRepeatedElements :: Ord a => [a] -> String -> Either String ()
notRepeatedElements list errorMessage =
  if List.Unique.allUnique list then Right () else Left errorMessage

-- JSON representation is just the list of HexColor
-- Warning: Not use Bimap.keysR, obs that:
-- "Return all right-hand keys in the bimap in ascending order"
instance ToJSON SimplePalette256 where
  toJSON (SimplePalette256 bimap) = toJSON $ snd <$> Bimap.toList bimap

-- TODO: Check that parsing is also respecting the array order!
instance FromJSON SimplePalette256 where
  parseJSON (Array xs) = do
    colors <- mapM parseJSON $ Data.Foldable.toList xs
    case mkSimplePalette256 colors of
      Left errorMessage -> fail $ "Decode SimplePalette256: " <> errorMessage
      Right palette -> return palette
  parseJSON _ = fail "Decode SimplePalette256: Expected an array of HexColors"

colorBSWith :: Palette p => p -> ByteString -> Maybe [HexColor]
colorBSWith palette bs = do
  let bytes = unpack bs
      x = (\byte -> lookupHexColor palette byte) <$> bytes
      mEncodedBytes = sequence x
  mEncodedBytes

colorFileWith :: (MonadUnliftIO m, Palette p) => p -> FilePath -> m (Maybe [HexColor])
colorFileWith palette sourceFilePath = do
  let color = colorBSWith palette
  chunks <-
    runConduitRes $
      sourceFileBS sourceFilePath .| mapC color .| sinkList
  let mColoredChunks = sequence chunks
  let mColoredFile = join <$> mColoredChunks
  return mColoredFile

-- | TODO: Create something like: Painter
colorFileWith' :: (MonadUnliftIO m) => FilePath -> m (Maybe [HexColor])
colorFileWith' sourceFilePath = do
  let color :: ByteString -> Maybe HexColor
      color = eitherToMaybe . parseHexColorFromRGB
  chunks <-
    runConduitRes $
      sourceFileBS sourceFilePath .| chunksOfE 3 .| mapC color .| sinkList
  let mColoredFile = sequence chunks
  return mColoredFile

colorFileWith'' :: (MonadUnliftIO m) => FilePath -> m (Maybe [HexColor])
colorFileWith'' sourceFilePath = do
  let color :: ByteString -> Maybe HexColor
      color = eitherToMaybe . parseHexColorFromRGBA
  chunks <-
    runConduitRes $
      sourceFileBS sourceFilePath .| chunksOfE 4 .| mapC color .| sinkList
  let mColoredFile = sequence chunks
  return mColoredFile