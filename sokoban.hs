{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld        -- cabal install codeworld-api

main :: IO ()
main = drawingOf pictureOfMaze

-- different squares that may occur in Sokoban
wall :: Picture
wall = (colored (dark gray) (solidRectangle 1 1))

ground :: Picture
ground = (colored (yellow) (solidRectangle 1 1))

storage :: Picture
storage = (solidCircle 0.3) & (colored (yellow) (solidRectangle 1 1))

box :: Picture
box = (colored (brown) (solidRectangle 1 1))

drawTile :: Int -> Picture
drawTile index
    | index == 1 = wall
    | index == 2 = ground
    | index == 3 = storage 
    | index == 4 = box
    | otherwise = blank

maze :: Int -> Int -> Int
maze x y
    | abs x > 4  || abs y > 4  = 0
    | abs x == 4 || abs y == 4 = 1
    | x ==  2 && y <= 0        = 1
    | x ==  3 && y <= 0        = 3
    | x >= -2 && y == 0        = 4
    | otherwise                = 2

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank (map pictureOfMazeRow [-10..10])

pictureOfMazeRow :: Int -> Picture
pictureOfMazeRow x = foldl (&) blank (map (pictureOfMazeRowCol x) [-10..10])

pictureOfMazeRowCol :: Int -> Int -> Picture
pictureOfMazeRowCol x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))