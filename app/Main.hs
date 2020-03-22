-- step - 1

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld        -- cabal install codeworld-api

data Location = Loc Int Int deriving Eq
data Direction = RightDir | UpDir | LeftDir | DownDir
data ListOf a = Empty | Entry a (ListOf a) deriving Show
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data State = St Location Direction (ListOf Location)

-- step - 2

-- main :: IO ()
-- main = drawingOf (pictureOfBoxes (boxesOfMaze maze))

pictureOfBoxes :: (ListOf Location) -> Picture
pictureOfBoxes Empty = blank
pictureOfBoxes (Entry loc cs) = atLocation loc (drawTile Box) & (pictureOfBoxes cs)

atLocation :: Location -> Picture -> Picture
atLocation (Loc x y) pic = translated (fromIntegral x) (fromIntegral y) pic

-- different squares that may occur in Sokoban
wall :: Picture
wall = (colored (dark gray) (solidRectangle 1 1))

ground :: Picture
ground = (colored (yellow) (solidRectangle 1 1))

storage :: Picture
storage = (solidCircle 0.3) & (colored (yellow) (solidRectangle 1 1))

box :: Picture
box = (colored (brown) (solidRectangle 1 1))

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

startState :: State
startState = St (Loc 0 1) UpDir (boxesOfMaze maze)  -- level 1
-- startState = Join (C 3 4) D  -- level 2

appendList :: ListOf a -> ListOf a -> ListOf a
appendList Empty l = l
appendList (Entry x l1) l2 = Entry x (appendList l1 l2)

boxesOfMaze :: (Location -> Tile) -> (ListOf Location)
boxesOfMaze m = foldl (appendList) Empty (map (boxesOfMazeRow m) [-10..10])

boxesOfMazeRow :: (Location -> Tile) -> Int -> (ListOf Location)
boxesOfMazeRow m x = foldl (appendList) Empty (map (boxesOfMazeRowCol m x) [-10..10])

boxesOfMazeRowCol :: (Location -> Tile) -> Int -> Int -> (ListOf Location)
boxesOfMazeRowCol m x y
    | m (Loc x y) == Box    = Entry (Loc x y) Empty
    | otherwise             = Empty

maze :: Location -> Tile
maze = mazeL1

mazeL1 :: Location -> Tile                  -- level 1
mazeL1 (Loc x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

-- step 3

-- main :: IO ()
-- main = drawingOf pictureOfMaze

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank (map pictureOfMazeRow [-10..10])

pictureOfMazeRow :: Int -> Picture
pictureOfMazeRow x = foldl (&) blank (map (pictureOfMazeRowCol x) [-10..10])

pictureOfMazeRowCol :: Int -> Int -> Picture
pictureOfMazeRowCol x y = translated (fromIntegral x) (fromIntegral y) (drawTile (noBoxMaze (Loc x y)))

noBoxMaze :: Location -> Tile
noBoxMaze c = noBox (maze c)
    where
        noBox :: Tile -> Tile
        noBox Box = Ground
        noBox t   = t

mazeWithBoxes :: (ListOf Location) -> Location -> Tile
mazeWithBoxes boxes loc 
    | inList boxes loc  = Box
    | otherwise         = noBoxMaze loc

inList :: Eq a => ListOf a -> a -> Bool
inList Empty _ = False
inList (Entry a1 l1) a2 = (a1 == a2) || (inList l1 a2)

-- step 4

main :: IO ()
main = drawingOf (draw startState)

draw :: State -> Picture
draw (St loc dir boxes) = (atLocation loc (directedPlayer dir)) & (pictureOfBoxes boxes) & pictureOfMaze 

-- make player fancier later
player :: Picture
player = (colored green (translated 0 0.3 (solidRectangle 0.1 0.3))) & (colored pink (solidCircle 0.3))

directedPlayer :: Direction -> Picture
directedPlayer UpDir = rotated (0.0 * pi) player
directedPlayer LeftDir = rotated (0.5 * pi) player
directedPlayer DownDir = rotated (1.0 * pi) player
directedPlayer RightDir = rotated (1.5 * pi) player

-- step 5