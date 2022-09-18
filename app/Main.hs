module Main where

-- import qualified ColorinColorado.Svg.SingleLineSvgCodec as SingleLineSvgCodec (main)
import qualified ColorinColorado.Svg.QuadraticMatrixSvgCodec as QuadraticMatrixSvgCodec (mainRGBA)

-- Execution example:
-- cabal v2-run colorin-colorado -- ./palette/windows95-shuffle.json ./encode-example
-- >> ./encode-example.svg 
main :: IO ()
main = QuadraticMatrixSvgCodec.mainRGBA
