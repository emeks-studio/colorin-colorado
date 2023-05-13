{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.Types.Colors
  ( HexColor,
    toText,
    parseHexColorFromText, -- Smart constructor for HexColor
    parseHexColorFromRGB,
    parseHexColorFromRGBA,
    fromRGB,
    fromRGBA,
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
  )
import qualified Data.ByteString as BS (ByteString, unpack)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Numeric (showHex)
import Text.Regex.TDFA ((=~))

hexColorRegexValidation :: String
hexColorRegexValidation = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{8})$"

newtype HexColor = HexColor Text
  deriving stock (Generic, Eq, Ord)

toText :: HexColor -> Text
toText (HexColor t) = t

parseHexColorFromText :: Text -> Either String HexColor
parseHexColorFromText v = do
  let unpacked = Text.unpack v
  if (unpacked =~ hexColorRegexValidation :: Bool)
    then return $ HexColor v
    else Left "Invalid string"

fromRGB :: (Word8, Word8, Word8) -> HexColor
fromRGB (r, g, b) =
  -- Prepend 0 if Numeric.showHex returns only 1 digit,
  -- then take 2 digits (in case there were already 2 digits)
  let r' = take 2 $ Numeric.showHex r "0"
      g' = take 2 $ Numeric.showHex g "0"
      b' = take 2 $ Numeric.showHex b "0"
      hexColor = "#" <> r' <> g' <> b'
   in HexColor $ Text.pack hexColor

parseHexColorFromRGB :: BS.ByteString -> Either String HexColor
parseHexColorFromRGB bs =
  case BS.unpack bs of
    [r, g, b] -> Right $ fromRGB (r, g, b)
    [r, g] -> Right $ fromRGB (r, g, defaultValue)
    [r] -> Right $ fromRGB (r, defaultValue, defaultValue)
    [] -> Left "Empty Bytestring"
    _ -> Left "Bytestring too large"
  where
    defaultValue :: Word8
    defaultValue = fromIntegral (255 :: Integer)

fromRGBA :: (Word8, Word8, Word8, Word8) -> HexColor
fromRGBA (r, g, b, a) =
  -- Prepend 0 if Numeric.showHex returns only 1 digit,
  -- then take 2 digits (in case there were already 2 digits)
  let r' = take 2 $ Numeric.showHex r "0"
      g' = take 2 $ Numeric.showHex g "0"
      b' = take 2 $ Numeric.showHex b "0"
      a' = take 2 $ Numeric.showHex a "0"
      hexColor = "#" <> r' <> g' <> b' <> a'
   in HexColor (Text.pack hexColor)

parseHexColorFromRGBA :: BS.ByteString -> Either String HexColor
parseHexColorFromRGBA bs =
  case BS.unpack bs of
    [r, g, b, a] -> Right $ fromRGBA (r, g, b, a)
    [r, g, b] -> Right $ fromRGBA (r, g, b, defaultValue)
    [r, g] -> Right $ fromRGBA (r, g, defaultValue, defaultValue)
    [r] -> Right $ fromRGBA (r, defaultValue, defaultValue, defaultValue)
    [] -> Left "Empty Bytestring"
    _ -> Left "Bytestring too large"
  where
    defaultValue :: Word8
    defaultValue = fromIntegral (255 :: Integer)

-- (!): Display color with extra quotes!
instance Show HexColor where
  show (HexColor t) = show t

instance ToJSON HexColor where
  toJSON (HexColor t) = toJSON t

instance FromJSON HexColor where
  parseJSON (String v) = case parseHexColorFromText v of
    Right color -> return color
    Left errorMessage -> fail $ "Decode HexColor: " <> errorMessage
  parseJSON _ = fail "Decode HexColor: Expected an string"
