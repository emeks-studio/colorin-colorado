module Main where

import ColorinColorado.Types.Palette (mkSimplePalette256)
import ColorinColorado.Utils (getOrThrow)
import System.Environment (getArgs)
import Data.Aeson (toJSON, eitherDecodeFileStrict', encodeFile)
import System.Random.Shuffle (shuffleM)

-- Execution example:
-- cabal v2-run gen-shuffle-palette -- ./palette/windows95-ordered.json ./palette/windows95-shuffle.json
main :: IO ()
main = do
  [existingPalettePath, newPalettePath] <- getArgs
  colors <- getOrThrow $ eitherDecodeFileStrict' existingPalettePath
  colorsShuffled <- shuffleM colors
  palette <- getOrThrow . pure $ mkSimplePalette256 colorsShuffled
  let paletteJson = toJSON palette
  encodeFile newPalettePath paletteJson
