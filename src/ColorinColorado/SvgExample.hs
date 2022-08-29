{-# LANGUAGE OverloadedStrings #-}

-- | More docs at https://github.com/diagrams/svg-builder
module ColorinColorado.SvgExample where

import Control.Exception (catch, throwIO)
import System.Environment (getArgs)
import System.IO.Error (isDoesNotExistError)
import Graphics.Svg
    ( (->>),
      (<<-),
      with,
      doctype,
      rect_,
      svg11_,
      AttrTag(Fill_, Version_, Width_, Height_, X_),
      Element )
import System.Directory ( removeFile )

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
  tryRemoveFileIfExist filenameInCurrentDirectoryPath
  writeFile filenameInCurrentDirectoryPath svgContent 

tryRemoveFileIfExist :: FilePath -> IO ()
tryRemoveFileIfExist path =
  removeFile path `catch` \e -> if isDoesNotExistError e then return () else throwIO e