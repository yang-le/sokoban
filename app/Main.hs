{-# LANGUAGE OverloadedStrings #-}

import CodeWorld
import Types
import Player
import Mazes

initialBoxes :: (Coord -> Tile) -> List Coord
initialBoxes m = do4Maze Empty findBox appendList where
    findBox c = case m c of
        Box -> Entry c Empty
        _ -> Empty

initialState :: State
initialState = loadLevel 1

loadLevel :: Integer -> State
loadLevel l = go (nth mazes l) where
    go (Maze c m) = S l c R (initialBoxes m)

pictureOfMaze :: (Coord -> Tile) -> Picture
pictureOfMaze m = do4Maze blank (drawTileAt m) (&)

do21times :: a -> (Integer -> a) -> (a -> a -> a) -> a
do21times init something fold = go (-10) where
    go 11 = init
    go n = fold (something n) (go (n + 1))

do4Maze :: a -> (Coord -> a) -> (a -> a -> a) -> a
do4Maze init coordOp fold = do21times init (\r -> do21times init (coordOp . C r) fold) fold

drawTileAt :: (Coord -> Tile) -> Coord -> Picture
drawTileAt m c = atCoord c (drawTile (noBoxMaze m c))

noBoxMaze :: (Coord -> Tile) -> Coord -> Tile
noBoxMaze m c = case m c of
    Box -> Ground
    t -> t

mazeWithBoxes :: (Coord -> Tile) -> List Coord -> Coord -> Tile
mazeWithBoxes m Empty c = noBoxMaze m c
mazeWithBoxes m (Entry c cs) c'
    | c == c'   = Box
    | otherwise = mazeWithBoxes m cs c'

canMove :: (Coord -> Tile) -> Direction -> Coord -> List Coord -> Bool
canMove m d c b = t == Ground || t == Storage
    || (t == Box && (tt == Ground || tt == Storage))
    where   t = mazeWithBoxes m b c
            tt = mazeWithBoxes m b (adjacentCoord d c)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo from to adj
    | from == adj   = to
    | otherwise     = adj

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) (S l c d b)
    | isWon m b
    , key == " "
    , l < listLength mazes  = loadLevel (l + 1)
    | isWon m b             = S l c d b
    | key == "Right"        = tryGoto (S l c R b)
    | key == "Up"           = tryGoto (S l c U b)
    | key == "Left"         = tryGoto (S l c L b)
    | key == "Down"         = tryGoto (S l c D b)
    where m = nthMaze l
handleEvent _ s = s

tryGoto :: State -> State
tryGoto (S l c d b)
    | canMove m d dest b = S l dest d nb
    | otherwise = S l c d b
    where   m = nthMaze l
            dest = adjacentCoord d c
            nb = mapList (moveFromTo dest (adjacentCoord d dest)) b

drawState :: State -> Picture
drawState (S l c d b)
    | isWon m b
    , l == listLength mazes = scaled 3 3 (lettering "All done!") & drawGame
    | isWon m b             = scaled 3 3 (lettering "You won!") & drawGame
    | otherwise             = drawGame
    where   m = nthMaze l
            drawGame = atCoord c (player d) & pictureOfBoxes b & pictureOfMaze m

sokoban :: Activity State
sokoban = Activity initialState handleEvent drawState

nthMaze :: Integer -> (Coord -> Tile)
nthMaze l = go (nth mazes l) where
    go (Maze _ m) = m

main :: IO ()
main = runActivity (withStartScreen (withUndo (resetable sokoban)))

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

isWon :: (Coord -> Tile) -> List Coord -> Bool
isWon m b = allList (mapList ((== Storage) . m) b)
