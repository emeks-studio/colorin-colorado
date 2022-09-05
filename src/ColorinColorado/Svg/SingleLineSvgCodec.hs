{-# LANGUAGE OverloadedStrings #-}

module ColorinColorado.Svg.SingleLineSvgCodec (main) where

import ColorinColorado.Svg.Codec (SvgGeneratorFn)
import ColorinColorado.Svg.Common (mkRect, mkSvg)
import ColorinColorado.Types.Palette
  ( Palette,
    SimplePalette256,
    colorFileWith,
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
      -- TODO: Use a better function (FIXME: Too large list can break!)
      totalWidth = totalLength * 10
      totalHeight = 400
      widthPerElement = 10
      (_currentPosition, content) =
        List.foldl'
          ( \acc color ->
              let currentContent = snd acc
                  currentPosition = fst acc
                  newContent = mkRect currentPosition widthPerElement totalHeight color
               in (currentPosition + widthPerElement, currentContent <> newContent)
          )
          (0, mempty)
          colors
   in mkSvg totalWidth totalHeight content

encode :: (MonadUnliftIO m, Palette p) => p -> SvgGeneratorFn -> FilePath -> m ()
encode palette svgGeneratorFn sourceFilePath = do
  mColors <- colorFileWith palette sourceFilePath
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
