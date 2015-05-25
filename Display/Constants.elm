module Display.Constants where

import Color

gameMargin = 16
sidebarWidth = 582
sidebarImageHeight = 82
sidebarRightAreaHeight = 282
handPadding = 10
handTileSize = 100

minSidebarHeight = 2 * (handPadding + handTileSize) + sidebarImageHeight + sidebarRightAreaHeight + 2 * 11 + 2 * 19 

transparent = Color.rgba 0 0 0 0.0
transpGreen = Color.rgba 0 255 0 0.5