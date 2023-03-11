{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.Svg.SingleLineSvgEncoder (SingleLineSvgGenerator (SingleLineSvgGenerator), main) where

import ColorinColorado.Svg.Common (mkRect, mkSvg)
import ColorinColorado.Svg.Encoder (SvgGenerator, encodeFile, genSvgFromColors)
import ColorinColorado.Types.Painter
  ( AnyPalette (AnyPalette),
    RGBAPainter (RGBAPainter),
    RGBPainter (RGBPainter),
  )
import ColorinColorado.Types.Palette (SimplePalette256)
import ColorinColorado.Utils (getOrThrow)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.List as List (foldl', length)
import System.Environment (getArgs)

data SingleLineSvgGenerator = SingleLineSvgGenerator

instance (SvgGenerator SingleLineSvgGenerator) where
  genSvgFromColors _ colors =
    let totalLength = List.length colors
        totalWidth = totalLength
        totalHeight = 1
        widthPerElement = 1
        startingXPosition = 0
        fixedYPosition = 0
        (_, content) =
          List.foldl'
            ( \acc color ->
                let (currentXPosition, currentContent) = acc
                    newContent = mkRect currentXPosition fixedYPosition widthPerElement totalHeight color
                 in (currentXPosition + widthPerElement, currentContent <> newContent)
            )
            (startingXPosition, mempty)
            colors
     in mkSvg totalWidth totalHeight content

main :: IO ()
main = do
  [palettePath, sourceFilePath] <- getArgs
  palette <- getOrThrow (eitherDecodeFileStrict' palettePath :: IO (Either String SimplePalette256))
  -- TODO: Choose between encoders!
  let singleLinePaletteSvgEncoder = (SingleLineSvgGenerator, AnyPalette palette)
  let _singleLineRGBSvgEncoder = (SingleLineSvgGenerator, RGBPainter)
  let _singleLineRGBASvgEncoder = (SingleLineSvgGenerator, RGBAPainter)
  mSvgElement <- encodeFile singleLinePaletteSvgEncoder sourceFilePath
  case mSvgElement of
    Nothing -> liftIO $ putStrLn "Error while trying to encode"
    Just svgElement -> do
      let writableSvg :: String
          writableSvg = show svgElement
      liftIO $ writeFile (sourceFilePath <> ".palette.svg") writableSvg

--  liftIO $ writeFile (sourceFilePath <> ".rgb.svg") writableSvg
--  liftIO $ writeFile (sourceFilePath <> ".rgba.svg") writableSvg
