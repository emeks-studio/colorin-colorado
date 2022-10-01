module Main where

-- import qualified ColorinColorado.Svg.SingleLineSvgEncoder as SingleLineSvgEncoder (main)
import qualified ColorinColorado.Svg.QuadraticMatrixSvgEncoder as QuadraticMatrixSvgEncoder (main)

-- Execution example:
-- cabal v2-run colorin-colorado -- ./palette/windows95-shuffle.json ./encode-example
-- >> ./encode-example.svg 
main :: IO ()
main = QuadraticMatrixSvgEncoder.main
