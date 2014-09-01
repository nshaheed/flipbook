{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Data.Active
import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 $ flip loop 0

loop :: DeviceContext -> Time -> IO ()
loop context n = let x = mkActive 0 1 $ rotateSquare context
                 in loopWorker x n

loopWorker :: Active (IO ()) -> Time -> IO ()
loopWorker x n = do
    runActive x n
    threadDelay (75 * 1000)
    loopWorker x (n + 0.01)

rotateSquare :: DeviceContext -> Time -> IO ()
rotateSquare context n = send context $ do
    let (w,h) = (width context, height context)
    clearRect (0,0,w,h)
    beginPath ()
    save ()
    translate (w / 2, h / 2)
    rotate $ pi * (fromTime n)
    beginPath ()
    moveTo(-100,-100)
    lineTo(-100,100)
    lineTo(100,100)
    lineTo(100,-100)
    closePath ()
    lineWidth 10
    strokeStyle "green"
    stroke ()
    restore ()