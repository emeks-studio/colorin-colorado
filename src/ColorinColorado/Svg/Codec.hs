module ColorinColorado.Svg.Codec
  ( SvgGeneratorFn,
  )
where

import qualified ColorinColorado.Types.Colors as Colors (HexColor)
import Graphics.Svg
  ( Element,
  )

type SvgGeneratorFn = [Colors.HexColor] -> Element
