{-# LANGUAGE OverloadedStrings #-}

module Main where

import ColorinColorado.Svg.Encoder (Encoder, encodeFile)
import ColorinColorado.Svg.QuadraticMatrixSvgEncoder (QuadraticSvgMatrixGenerator (QuadraticSvgMatrixGenerator))
import ColorinColorado.Svg.SingleLineSvgEncoder (SingleLineSvgGenerator (SingleLineSvgGenerator))
import ColorinColorado.Types.Painter
  ( AnyPalette (AnyPalette),
    Painter,
    RGBAPainter (RGBAPainter),
    RGBPainter (RGBPainter),
  )
import ColorinColorado.Types.Palette (SimplePalette256)
import ColorinColorado.Utils (getOrThrow)
import Data.Aeson (eitherDecodeFileStrict')
import Data.Text (Text)
import Turtle (Parser, argPath, optText, optional, options)

data EncoderParams = EncoderParams
  { shape :: Text,
    format :: Text,
    src :: FilePath,
    palleteFile :: Maybe FilePath
  }

encoderParamsParser :: Parser EncoderParams
encoderParamsParser =
  EncoderParams
    <$> optText "shape" 's' "Image shape. ptions: matrix | line"
    <*> optText "format" 'f' "Image format. options: rgb | rgba | palette PALETTE_FILE"
    <*> argPath "src" "The source file path to encode"
    <*> optional (argPath "PALETTE_FILE" "If you choose format palette you must pass a palette spec file (expected format .json)")

-- example: cabal run colorin-colorado -- -s matrix -f rgba "./flake.nix"
main :: IO ()
main = do
  encoderParams <- options "imageEncoder allows you to encode any file into different types of images" encoderParamsParser
  case (format encoderParams, palleteFile encoderParams) of
    ("pallete", Just palleteFile') -> do
      palette <- getOrThrow (eitherDecodeFileStrict' palleteFile' :: IO (Either String SimplePalette256))
      handleEncoding (AnyPalette palette) ".palette" encoderParams
    ("pallete", Nothing) -> putStrLn "pallete format expects a palleteFile"
    ("rgb", Just _) -> putStrLn "rgb format does not support a custom pallete"
    ("rgb", Nothing) -> handleEncoding RGBPainter ".rgb" encoderParams
    ("rgba", Just _) -> putStrLn "rgba format does not support a custom pallete"
    ("rgba", Nothing) -> handleEncoding RGBAPainter ".rgba" encoderParams
    (_, _) -> putStrLn "invalid format option"

handleEncoding :: Painter a => a -> String -> EncoderParams -> IO ()
handleEncoding painter encodingSuffix params = case shape params of
  "matrix" -> encode quadraticMatrixPaletteSvgEncoder (encodingSuffix <> ".matrix") (src params)
    where
      quadraticMatrixPaletteSvgEncoder = (QuadraticSvgMatrixGenerator, painter)
  "line" -> encode singleLineSvgEncoder (encodingSuffix <> ".line") (src params)
    where
      singleLineSvgEncoder = (SingleLineSvgGenerator, painter)
  _ -> putStrLn "invalid shape option"

encode :: Encoder a => a -> String -> FilePath -> IO ()
encode encoder encodingSuffix sourceFilePath = do
  mSvgElement <- encodeFile encoder sourceFilePath
  case mSvgElement of
    Nothing -> putStrLn "Error while trying to encode"
    Just svgElement -> do
      let writableSvg :: String
          writableSvg = show svgElement
      writeFile (sourceFilePath <> encodingSuffix <> ".svg") writableSvg
