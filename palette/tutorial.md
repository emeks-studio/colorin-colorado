# Custom Palettes

"8-bit" (customizable) palettes uses as serialization format .json files;
Specifically an array of 256 hexagecimal (unique) colors where according to the position, 
each ASCII code will be replaced by the given color.

## About handmade.json

This palette is based on "windows95-ordered.json" palette but with a few modifications:

- greyscale for control characters (0-31 + 127)
- white for "space"
- black for "nbsp"
- "highlighted" colors for numbers (see palette/examples/encode-example.palette.line.svg)

Refs.

- Windows95 Palette: https://lospec.com/palette-list/windows-95-256-colours
- ASCII TABLE REFERENCE: https://theasciicode.com.ar/ascii-printable-characters/at-sign-atpersand-ascii-code-64.html
- Useful colors for custom palettes: https://htmlcolorcodes.com/color-chart/