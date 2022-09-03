{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module ColorinColorado.Types.Palette
  ( Palette, -- General purpose interface
    lookupHexColor,
    lookupByte,
    SimplePalette256, -- Specific implementation
    mkSimplePalette256, -- Smart constructor for SimplePalette256
  )
where

import ColorinColorado.Types.Colors (HexColor)
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
