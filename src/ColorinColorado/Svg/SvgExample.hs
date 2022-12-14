{-# LANGUAGE OverloadedStrings #-}

-- | More docs at https://github.com/diagrams/svg-builder
module ColorinColorado.Svg.SvgExample where

import System.Environment (getArgs)
import Graphics.Svg
    ( (->>),
      (<<-),
      with,
      doctype,
      rect_,
      svg11_,
      AttrTag(Fill_, Version_, Width_, Height_, X_),
      Element )

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "300", Height_ <<- "300"]

contents :: Element
contents =
     rect_   [ Width_ <<- "50", Height_ <<- "50", "#000080" ->> Fill_]
  <> rect_   [ X_ <<- "50", Width_ <<- "50", Height_ <<- "50", "#800000" ->> Fill_]
  <> rect_   [ X_ <<- "100", Width_ <<- "50", Height_ <<- "50", "#808000" ->> Fill_]

-- | Example: cabal v2-run colorin-colorado -- example.svg
main :: IO ()
main = do
  [filename] <- getArgs
  let filenameInCurrentDirectoryPath :: FilePath
      filenameInCurrentDirectoryPath = "./" ++ filename
      svgContent :: String
      svgContent = show $ svg contents
  -- Obs: Completly overrides the existing file
  writeFile filenameInCurrentDirectoryPath svgContent 
