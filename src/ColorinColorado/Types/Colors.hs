{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module ColorinColorado.Types.Colors
  ( HexColor,
    asText,
    mkHexColor, -- Smart constructor for HexColor
  )
where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
  )
import Data.Text (Text, unpack)
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

hexColorRegexValidation :: String
hexColorRegexValidation = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$"

newtype HexColor = HexColor Text
  deriving stock (Generic, Eq, Ord)

asText :: HexColor -> Text
asText (HexColor t) = t

mkHexColor :: Text -> Either String HexColor
mkHexColor v = do
  let unpacked = unpack v
  if (unpacked =~ hexColorRegexValidation :: Bool)
    then return $ HexColor v
    else Left "Invalid string"

instance Show HexColor where
  show (HexColor t) = show t

instance ToJSON HexColor where
  toJSON (HexColor t) = toJSON t

instance FromJSON HexColor where
  parseJSON (String v) = case mkHexColor v of
    Right color -> return color
    Left errorMessage -> fail $ "Decode HexColor: " <> errorMessage
  parseJSON _ = fail "Decode HexColor: Expected an string"
