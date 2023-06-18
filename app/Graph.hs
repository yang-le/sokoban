module Graph where

import Types
import Mazes

isGraphClosed :: Eq a => a -> (a -> List a) -> (a -> Bool) -> Bool
isGraphClosed initial adjacent isOk = go Empty (adjacent initial) where
    go _ Empty = True
    go seen (Entry e es)
        | elemList e seen   = go seen es
        | not (isOk e)      = False
        | otherwise         = go (Entry e seen) (appendList (adjacent e) es)

isClosed :: Maze -> Bool
isClosed (Maze c m) = (m c == Ground || m c == Storage)
    && isGraphClosed c adjacent ((/= Blank) . m) where
        adjacent x = filterList ((/= Wall) . m) (mapList (`adjacentCoord` x) allDirections)
