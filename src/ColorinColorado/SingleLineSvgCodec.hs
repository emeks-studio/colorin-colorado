{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.SingleLineSvgCodec (main) where

import ColorinColorado.Types.Colors (HexColor, asText)
import ColorinColorado.Types.Palette (Palette, SimplePalette256, lookupHexColor)
import ColorinColorado.Utils (getOrThrow)
import Conduit
  ( mapC,
    runConduitRes,
    sinkList,
    sourceFileBS,
    (.|),
  )
import Control.Monad (join)
import Data.Aeson (eitherDecodeFileStrict')
import Data.ByteString (ByteString, unpack)
import qualified Data.List as List (foldl', length)
import Data.Text ( pack, Text )
import Graphics.Svg
  ( AttrTag (Fill_, Height_, Version_, Width_, X_),
    Element,
    doctype,
    rect_,
    svg11_,
    with,
    (->>),
    (<<-),
  )
import System.Environment (getArgs)

toText :: Show a => a -> Text
toText x = pack $ show x

colorBS :: Palette p => p -> ByteString -> Maybe [HexColor]
colorBS palette bs = do
  let bytes = unpack bs
      x = (\byte -> lookupHexColor palette byte) <$> bytes
      mEncodedBytes = sequence x
  mEncodedBytes

svg :: Int -> Element -> Element
svg size content =
  let totalWidth = size * 50
   in doctype
        <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- toText totalWidth, Height_ <<- "400"]

-- FIXME: Seems like (pack . show $ color) is printing color with extra quotes
-- FIXME: Some files (large ones?) are breaking!
-- FIXME: Rename & Refactor
magic :: [HexColor] -> Element
magic colors =
  let content :: (Int, Element)
      content =
        List.foldl'
          ( \acc color ->
              let currentPosition = fst acc
                  currentContent = snd acc
                  newContent = rect_ [X_ <<- toText currentPosition, Width_ <<- "50", Height_ <<- "400", asText color ->> Fill_]
               in (currentPosition + 50, currentContent <> newContent)
          )
          (0, mempty)
          colors
   in svg (List.length colors) (snd content)

main :: IO ()
main = do
  [palettePath, fileToEncodePath] <- getArgs
  palette <- getOrThrow (eitherDecodeFileStrict' palettePath :: IO (Either String SimplePalette256))
  let color = colorBS palette
  -- TODO: We could do better!
  res <-
    runConduitRes $
      sourceFileBS fileToEncodePath .| mapC color .| sinkList
  let res' = sequence res
  case res' of
    Nothing -> putStrLn "Error while trying to encode"
    Just res'' -> do
      let allColors = join res''
      _ <- putStrLn $ show allColors
      let encodedSvg = magic allColors
          printableSvg :: String
          printableSvg = show encodedSvg
      writeFile (fileToEncodePath <> ".svg") printableSvg
