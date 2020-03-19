{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld        -- cabal install codeworld-api

main :: IO ()
main = activityOf initialLocation handleEvent drawState

handleEvent :: Event -> Location -> Location
handleEvent (KeyPress key) c
    | key == "Right" = adjacentLocation R c
    | key == "Up"    = adjacentLocation U c
    | key == "Left"  = adjacentLocation L c
    | key == "Down"  = adjacentLocation D c
handleEvent _ c      = c

drawState :: Location -> Picture
drawState c = atLocation c pictureOfMaze

data Tile = Wall | Ground | Storage | Box | Blank
data Direction = R | U | L | D
data Location = C Int Int

initialLocation :: Location
initialLocation = C 0 0

atLocation :: Location -> Picture -> Picture
atLocation (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentLocation :: Direction -> Location -> Location
adjacentLocation R (C x y) = C  (x+1)   y
adjacentLocation U (C x y) = C  x       (y+1)
adjacentLocation L (C x y) = C  (x-1)   y
adjacentLocation D (C x y) = C  x       (y-1)

someLocation :: Location
someLocation = adjacentLocation U (adjacentLocation U (adjacentLocation L initialLocation))

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

maze :: Location -> Tile
maze (C x y)
    | abs x > 4  || abs y > 4  = Blank
    | abs x == 4 || abs y == 4 = Wall
    | x ==  2 && y <= 0        = Wall
    | x ==  3 && y <= 0        = Storage
    | x >= -2 && y == 0        = Box
    | otherwise                = Ground

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank (map pictureOfMazeRow [-10..10])

pictureOfMazeRow :: Int -> Picture
pictureOfMazeRow x = foldl (&) blank (map (pictureOfMazeRowCol x) [-10..10])

pictureOfMazeRowCol :: Int -> Int -> Picture
pictureOfMazeRowCol x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze (C x y)))

-- player :: Picture

