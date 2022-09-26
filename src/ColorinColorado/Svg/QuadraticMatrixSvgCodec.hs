{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.Svg.QuadraticMatrixSvgCodec (main, mainRGB, mainRGBA) where

import ColorinColorado.Svg.Codec (SvgGeneratorFn)
import ColorinColorado.Svg.Common (mkRect, mkSvg)
import ColorinColorado.Types.Palette
  ( Palette,
    SimplePalette256
  )
import ColorinColorado.Types.Painter
  ( coloringFile, 
    AnyPalette (AnyPalette),
    RGBPainter (RGBPainter),
    RGBAPainter (RGBAPainter)
  )
import ColorinColorado.Utils (getOrThrow)
import Conduit (MonadUnliftIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.List as List (foldl', length)
import System.Environment (getArgs)

quadraticSVGMatrix :: SvgGeneratorFn
quadraticSVGMatrix colors =
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
  if nextXPost < factor then
    (nextXPost, yPos)
  else
    (0, yPos + heightPerElement)
  where nextXPost = xPos + widthPerElement

-- TODO: As with Painter, create a Codec typeclass; Try to make SvgGeneratorFns and Painters composable
-- TODO: Use a logger with different levels: DEBUG, ERROR, etc
encode :: (MonadUnliftIO m, Palette p) => p -> SvgGeneratorFn -> FilePath -> m ()
encode palette svgGeneratorFn sourceFilePath = do
  mColors <- coloringFile (AnyPalette palette) sourceFilePath
  case mColors of
    Nothing -> liftIO $ putStrLn "Error while trying to encode"
    Just allColors -> do
      _ <- liftIO $ print allColors
      let totalLength = List.length allColors
          quadraticFactor :: Int
          quadraticFactor = ceiling $ sqrt (fromIntegral totalLength :: Double)
      _ <- liftIO $ print totalLength
      _ <- liftIO $ print quadraticFactor
      let encodedSvg = svgGeneratorFn allColors
          writableSvg :: String
          writableSvg = show encodedSvg
      liftIO $ writeFile (sourceFilePath <> ".svg") writableSvg

main :: IO ()
main = do
  [palettePath, sourceFilePath] <- getArgs
  palette <- getOrThrow (eitherDecodeFileStrict' palettePath :: IO (Either String SimplePalette256))
  encode palette quadraticSVGMatrix sourceFilePath

encodeRGB :: (MonadUnliftIO m) => SvgGeneratorFn -> FilePath -> m ()
encodeRGB svgGeneratorFn sourceFilePath = do
  mColors <- coloringFile RGBPainter sourceFilePath
  case mColors of
    Nothing -> liftIO $ putStrLn "Error while trying to encode"
    Just allColors -> do
      _ <- liftIO $ print allColors
      let totalLength = List.length allColors
          quadraticFactor :: Int
          quadraticFactor = ceiling $ sqrt (fromIntegral totalLength :: Double)
      _ <- liftIO $ print totalLength
      _ <- liftIO $ print quadraticFactor
      let encodedSvg = svgGeneratorFn allColors
          writableSvg :: String
          writableSvg = show encodedSvg
      liftIO $ writeFile (sourceFilePath <> ".rgb.svg") writableSvg

mainRGB :: IO ()
mainRGB = do
  [sourceFilePath] <- getArgs
  encodeRGB quadraticSVGMatrix sourceFilePath

encodeRGBA :: (MonadUnliftIO m) => SvgGeneratorFn -> FilePath -> m ()
encodeRGBA svgGeneratorFn sourceFilePath = do
  mColors <- coloringFile RGBAPainter sourceFilePath
  case mColors of
    Nothing -> liftIO $ putStrLn "Error while trying to encode"
    Just allColors -> do
      _ <- liftIO $ print allColors
      let totalLength = List.length allColors
          quadraticFactor :: Int
          quadraticFactor = ceiling $ sqrt (fromIntegral totalLength :: Double)
      _ <- liftIO $ print totalLength
      _ <- liftIO $ print quadraticFactor
      let encodedSvg = svgGeneratorFn allColors
          writableSvg :: String
          writableSvg = show encodedSvg
      liftIO $ writeFile (sourceFilePath <> ".rgba.svg") writableSvg

mainRGBA :: IO ()
mainRGBA = do
  [sourceFilePath] <- getArgs
  encodeRGBA quadraticSVGMatrix sourceFilePath