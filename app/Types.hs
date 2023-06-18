{-# LANGUAGE OverloadedStrings #-}

module Types where

import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data Direction = R | U | L | D deriving Eq
data Coord = C Integer Integer deriving Eq
data List a = Empty | Entry a (List a) deriving Eq
data State = S Integer Coord Direction (List Coord) deriving Eq
data SSState world = StartScreen | Running world
data Activity world = Activity world (Event -> world -> world) (world -> Picture)
data WithUndo a = WithUndo a (List a) deriving Eq
data Maze = Maze Coord (Coord -> Tile)

-- Direction Functions

allDirections :: List Direction
allDirections = Entry R (Entry U (Entry L (Entry D Empty)))

-- Tile Functions

drawTile :: Tile -> Picture
drawTile Wall = colored grey (solidRectangle 1.0 1.0)
drawTile Ground = colored yellow (solidRectangle 1.0 1.0)
drawTile Storage = colored white (solidCircle 0.3) & drawTile Ground
drawTile Box = colored brown (solidRectangle 1.0 1.0)
drawTile Blank = blank

-- Coord Functions

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) = translated (fromInteger x) (fromInteger y)

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x + 1) y
adjacentCoord U (C x y) = C x (y + 1)
adjacentCoord L (C x y) = C (x - 1) y
adjacentCoord D (C x y) = C x (y - 1)

-- Activity Functions

resetable :: Activity world -> Activity world
resetable (Activity state0 handle draw) = Activity state0 handle' draw where
    handle' (KeyPress key) _
        | key == "Esc" = state0
    handle' e s  = handle e s

withStartScreen :: Activity world -> Activity (SSState world)
withStartScreen (Activity state0 handle draw) = Activity StartScreen handle' draw' where
    handle' (KeyPress key)  StartScreen
        | key == " "                    = Running state0
    handle' _               StartScreen = StartScreen
    handle' e               (Running s) = Running (handle e s)
    draw' StartScreen = startScreen
    draw' (Running s) = draw s
    startScreen = scaled 3 3 (lettering "Sokoban!")

withUndo :: Eq world => Activity world -> Activity (WithUndo world)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 Empty
    handle' (KeyPress key) (WithUndo s stack)
        | key == "U" = case stack of
            Entry s' stack' -> WithUndo s' stack'
            Empty           -> WithUndo s Empty
    handle' e (WithUndo s stack)
        | s' == s   = WithUndo s stack
        | otherwise = WithUndo s' (Entry s stack)
        where s' = handle e s
    draw' (WithUndo s _) = draw s

runActivity :: Activity world -> IO ()
runActivity (Activity state0 handle draw) = activityOf state0 handle draw

-- List Functions

allList :: List Bool -> Bool
allList (Entry b bs) = b && allList bs
allList Empty = True

mapList :: (t -> a) -> List t -> List a
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

appendList :: List a -> List a -> List a
appendList (Entry c cs) l = Entry c (appendList cs l)
appendList Empty l = l

elemList :: Eq a => a -> List a -> Bool
elemList a Empty = False
elemList a (Entry e es) = a == e || elemList a es

listLength :: List a -> Integer
listLength Empty = 0
listLength (Entry e es) = 1 + listLength es

filterList :: (a -> Bool) -> List a -> List a
filterList f Empty = Empty
filterList f (Entry e es)
    | f e       = Entry e (filterList f es)
    | otherwise = filterList f es

nth :: List a -> Integer -> a
nth Empty _ = error "list too short"
nth (Entry e es) 1 = e
nth (Entry e es) n = nth es (n - 1)
