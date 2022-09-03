module ColorinColorado.Types.Palette
  ( Palette,
    lookupHexColor,
    lookupByte,
    buildPalette
  )
where

import Data.Word (Word8)
import qualified Data.Bimap as Bimap (Bimap, lookup, lookupR, fromList)
import ColorinColorado.Types.Colors (HexColorCode)
import Data.List as List (length)
import Data.List.Unique as List.Unique (allUnique)

class Palette a where
    lookupHexColor :: a -> Word8 -> Maybe HexColorCode
    lookupByte :: a -> HexColorCode -> Maybe Word8

-- TODO: Implement toJSON AND fromJSON
newtype MyPalette = MyPalette (Bimap.Bimap Word8 HexColorCode)

instance Palette MyPalette where
    lookupHexColor (MyPalette palette) byte = Bimap.lookup byte palette
    lookupByte (MyPalette palette) color = Bimap.lookupR color palette

buildPalette :: [Word8] -> [HexColorCode] -> Either String MyPalette
buildPalette bytes colors = do
  _ <- has256Elements colors "Palette must have 256 colors"
  _ <- has256Elements bytes "Palette must have 256 bytes"
  _ <- notRepeatedElements colors "Palette must have all unique colors"
  _ <- notRepeatedElements bytes "Palette must have all unique bytes"
  let tupleList = zip bytes colors
  return $ MyPalette $ Bimap.fromList tupleList

has256Elements :: [a] -> String -> Either String ()
has256Elements list errorMessage =
  if List.length list == 256 then Right () else Left errorMessage

notRepeatedElements :: Ord a => [a] -> String -> Either String ()
notRepeatedElements list errorMessage =
  if List.Unique.allUnique list then Right () else Left errorMessage

-- TODO: buildPalette with shuffle possible bytes!
-- Use shuffleM :: MonadRandom m => [a] -> m [a]
-- ^ lib: random-shuffle

allBytes :: [Word8]
allBytes = [0 .. 255]
