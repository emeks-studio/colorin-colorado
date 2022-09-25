{-# LANGUAGE DeriveAnyClass #-}
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
    fromRGBA
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))
import qualified Numeric (showHex)
import qualified Data.Text as Text (pack, unpack)
import Data.Word (Word8)
import qualified Data.ByteString as BS (ByteString, unpack)

hexColorRegexValidation :: String
hexColorRegexValidation = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

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

-- FIXME: Sometimes r g b has only 1 digit
fromRGB :: (Word8, Word8, Word8) -> HexColor
fromRGB (r, g, b) =
  let r' = Numeric.showHex r
      g' = Numeric.showHex g
      b' = Numeric.showHex b
      hex = r' . g' . b' $ ""
      hexColor = "#" <> hex
  in HexColor $ Text.pack hexColor 

parseHexColorFromRGB :: BS.ByteString -> Either String HexColor
parseHexColorFromRGB bs =
 case BS.unpack bs of
   [r, g, b] -> Right $ fromRGB (r, g, b)
   [r, g] ->  Right $ fromRGB (r, g, defaultValue)
   [r] ->  Right $ fromRGB (r, defaultValue , defaultValue)
   [] -> Left "Empty Bytestring"
   _ -> Left "Bytestring too large"
  where 
    defaultValue :: Word8
    defaultValue = fromIntegral (255 :: Integer) 

-- FIXME: Sometimes r g b a has only 1 digit
fromRGBA :: (Word8, Word8, Word8, Word8) -> HexColor   
fromRGBA (r, g, b, a) =
  let r' = Numeric.showHex r
      g' = Numeric.showHex g
      b' = Numeric.showHex b
      a' = Numeric.showHex a
      hex = r' . g' . b' . a' $ ""
      hexColor = "#" <> hex
  in HexColor (Text.pack hexColor)

parseHexColorFromRGBA :: BS.ByteString -> Either String HexColor
parseHexColorFromRGBA bs =
 case BS.unpack bs of
   [r, g, b, a] -> Right $ fromRGBA (r, g, b, a)
--   _ -> Left "Bytestring with invalid length"
   [r, g, b] -> Right $ fromRGBA (r, g, b, defaultValue)
   [r, g] ->  Right $ fromRGBA (r, g, defaultValue, defaultValue)
   [r] ->  Right $ fromRGBA (r, defaultValue, defaultValue , defaultValue)
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
