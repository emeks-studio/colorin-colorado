{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.Svg.SingleLineSvgCodec (main) where

import ColorinColorado.Svg.Codec (SvgGeneratorFn)
import ColorinColorado.Svg.Common (mkRect, mkSvg)
import ColorinColorado.Types.Palette
  ( Palette,
    SimplePalette256,
  )
import ColorinColorado.Types.Painter
  ( coloringFile, 
    AnyPalette (AnyPalette)
  )
import ColorinColorado.Utils (getOrThrow)
import Conduit (MonadUnliftIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecodeFileStrict')
import qualified Data.List as List (foldl', length)
import System.Environment (getArgs)

singleSVGLine :: SvgGeneratorFn
singleSVGLine colors =
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

-- TODO: Use a logger with different levels: DEBUG, ERROR, etc
encode :: (MonadUnliftIO m, Palette p) => p -> SvgGeneratorFn -> FilePath -> m ()
encode palette svgGeneratorFn sourceFilePath = do
  mColors <- coloringFile (AnyPalette palette) sourceFilePath
  case mColors of
    Nothing -> liftIO $ putStrLn "Error while trying to encode"
    Just allColors -> do
      _ <- liftIO $ print allColors
      let encodedSvg = svgGeneratorFn allColors
          writableSvg :: String
          writableSvg = show encodedSvg
      liftIO $ writeFile (sourceFilePath <> ".svg") writableSvg

main :: IO ()
main = do
  [palettePath, sourceFilePath] <- getArgs
  palette <- getOrThrow (eitherDecodeFileStrict' palettePath :: IO (Either String SimplePalette256))
  encode palette singleSVGLine sourceFilePath
