-- step - 1

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld        -- cabal install codeworld-api

data Location = Loc Int Int
data Direction = Right | Up | Left | Down
data ListOf a = Empty | Entry a (ListOf a) deriving Show
data Tile = Wall | Ground | Storage | Box | Blank deriving Eq
data State = St Location Direction (ListOf Location)

-- step - 2

main :: IO ()
main = drawingOf (pictureOfBoxes (boxesOfMaze maze))

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
startState = St (Loc 0 1) Up (boxesOfMaze maze)  -- level 1
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