{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.Svg.QuadraticMatrixSvgEncoder (QuadraticSvgMatrixGenerator (..), main) where

import ColorinColorado.Svg.Common (mkRect, mkSvg)
import ColorinColorado.Svg.Encoder (SvgGenerator, encodeFile, genSvgFromColors)
import ColorinColorado.Types.Painter
  ( AnyPalette (AnyPalette),
    RGBAPainter (RGBAPainter),
    RGBPainter (RGBPainter),
  )
import ColorinColorado.Types.Palette
  ( SimplePalette256,
  )
import ColorinColorado.Utils (getOrThrow)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.List as List (foldl', length)
import System.Environment (getArgs)

data QuadraticSvgMatrixGenerator = QuadraticSvgMatrixGenerator

instance (SvgGenerator QuadraticSvgMatrixGenerator) where
  genSvgFromColors _ colors =
    let totalLength = List.length colors
        quadraticFactor = ceiling $ sqrt (fromIntegral totalLength :: Double)
        totalWidth = quadraticFactor
        totalHeight = quadraticFactor
        widthPerElement = 1
        heightPerElement = 1
        startingXPosition = 0
        startingYPosition = 0
        (_, _, content) =
          List.foldl'
            ( \acc color ->
                let (currentXPosition, currentYPosition, currentContent) = acc
                    newContent = mkRect currentXPosition currentYPosition widthPerElement heightPerElement color
                    (updatedXPosition, updatedYPosition) =
                      updatePositions currentXPosition currentYPosition widthPerElement heightPerElement quadraticFactor
                 in (updatedXPosition, updatedYPosition, currentContent <> newContent)
            )
            (startingXPosition, startingYPosition, mempty)
            colors
     in mkSvg totalWidth totalHeight content

updatePositions :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
updatePositions xPos yPos widthPerElement heightPerElement factor =
  if nextXPost < factor
    then (nextXPost, yPos)
    else (0, yPos + heightPerElement)
  where
    nextXPost = xPos + widthPerElement

-- TODO: Provide flags in order to decide which encoder use, etc, etc (in a different file)
main :: IO ()
main = do
  [palettePath, sourceFilePath] <- getArgs
  palette <- getOrThrow (eitherDecodeFileStrict' palettePath :: IO (Either String SimplePalette256))
  -- TODO: Choose between encoders!
  let quadraticMatrixPaletteSvgEncoder = (QuadraticSvgMatrixGenerator, AnyPalette palette)
  let _quadraticMatrixRGBSvgEncoder = (QuadraticSvgMatrixGenerator, RGBPainter)
  let _quadraticMatrixRGBASvgEncoder = (QuadraticSvgMatrixGenerator, RGBAPainter)
  mSvgElement <- encodeFile quadraticMatrixPaletteSvgEncoder sourceFilePath
  case mSvgElement of
    Nothing -> liftIO $ putStrLn "Error while trying to encode"
    Just svgElement -> do
      let writableSvg :: String
          writableSvg = show svgElement
      liftIO $ writeFile (sourceFilePath <> ".palette.svg") writableSvg

--  liftIO $ writeFile (sourceFilePath <> ".rgb.svg") writableSvg
--  liftIO $ writeFile (sourceFilePath <> ".rgba.svg") writableSvg
