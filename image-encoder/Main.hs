{-# LANGUAGE OverloadedStrings #-}

module Main where

import ColorinColorado.Svg.ElipticalSvgEncoder (ElipticalSvgEncoder (ElipticalSvgEncoder))
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
    <$> optText "shape" 's' "Image shape. Options: matrix | line | eliptical"
    <*> optText "format" 'f' "Image format. Options: rgb | rgba | palette PALETTE_FILE"
    <*> argPath "src" "The source file path to encode"
    <*> optional (argPath "PALETTE_FILE" "If you choose format palette you must pass a palette spec file (expected format .json)")

-- example: cabal run image-encoder -- -s matrix -f rgba "./flake.nix"
main :: IO ()
main = do
  encoderParams <- options "image-encoder allows you to encode any file into different types of images" encoderParamsParser
  case (format encoderParams, palleteFile encoderParams) of
    ("palette", Just palleteFile') -> do
      palette <- getOrThrow (eitherDecodeFileStrict' palleteFile' :: IO (Either String SimplePalette256))
      handleEncoding (AnyPalette palette) ".palette" encoderParams
    ("palette", Nothing) -> putStrLn "format option palette expects a PALETTE_FILE"
    ("rgb", Just _) -> putStrLn "format option rgb does not support a custom palette"
    ("rgb", Nothing) -> handleEncoding RGBPainter ".rgb" encoderParams
    ("rgba", Just _) -> putStrLn "format option rgba does not support a custom palette"
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
  "eliptical" -> encode elipticalSvgEncoder (encodingSuffix <> ".eliptical") (src params)
    where
      elipticalSvgEncoder = (ElipticalSvgEncoder, painter)
  _ -> putStrLn "invalid shape option"

encode :: Encoder a => a -> String -> FilePath -> IO ()
encode encoder encodingSuffix sourceFilePath = do
  mSvgElement <- encodeFile encoder sourceFilePath
  case mSvgElement of
    Nothing -> putStrLn "error while trying to encode!"
    Just svgElement -> do
      let writableSvg :: String
          writableSvg = show svgElement
      writeFile (sourceFilePath <> encodingSuffix <> ".svg") writableSvg
