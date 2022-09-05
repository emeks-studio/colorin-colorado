module Main where

import qualified ColorinColorado.Svg.SingleLineSvgCodec as SingleLineSvgCodec (main)

-- Execution example:
-- cabal v2-run colorin-colorado -- ./palette/windows95-shuffle.json ./default.nix
-- >> ./default.nix.svg 
main :: IO ()
main = SingleLineSvgCodec.main
