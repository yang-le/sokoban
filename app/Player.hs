module Player where

import CodeWorld
import Types

player :: Direction -> Picture
player R = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,0.05)]
          & polyline [(0,0),(0.3,-0.05)]
          & polyline [(0,-0.2),(0,0.1)]
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,0.05)]
          & polyline [(0,0),(-0.3,0.05)]
          & polyline [(0,-0.2),(0,0.1)]
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
          & polyline [(0,0),(0.3,-0.05)]
          & polyline [(0,0),(-0.3,-0.05)]
          & polyline [(0,-0.2),(0,0.1)]
          & polyline [(0,-0.2),(0.1,-0.5)]
          & polyline [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)
