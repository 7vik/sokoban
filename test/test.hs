{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main :: IO ()
main = exercise1

exercise1 :: IO ()
exercise1 =  animationOf myTrafficController

botCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))

topCircle :: Color -> Picture
topCircle c = colored c (translated 0   4.5  (solidCircle 1))

midCircle :: Color -> Picture
midCircle c = colored c (translated 0   1.5  (solidCircle 1))

frame :: Picture
frame = translated 0 1.2 (rectangle 2.5 8)

-- int denotes the state of the traffic lights
trafficLight :: Int -> Picture
trafficLight 1  = botCircle green & midCircle black & topCircle black & frame
trafficLight 2  = botCircle black & midCircle yellow & topCircle black & frame
trafficLight 3  = botCircle black & midCircle black & topCircle red & frame
trafficLight 4  = botCircle black & midCircle yellow & topCircle red & frame
trafficLight _ = botCircle black & midCircle black & topCircle black & frame

myTrafficController :: Double -> Picture 
myTrafficController t = myTrafficDriver (round (t) `mod` (6 :: Integer))
  
myTrafficDriver :: Integral a => a -> Picture
myTrafficDriver state 
  | state == 0 = trafficLight 1
  | state == 1 = trafficLight 1
  | state == 2 = trafficLight 2
  | state == 3 = trafficLight 3
  | state == 4 = trafficLight 3
  | state == 5 = trafficLight 4
  | otherwise = trafficLight 0