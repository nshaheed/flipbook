{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent

import Data.Active
import Data.Fixed

import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 $ flip loop 0

loop :: DeviceContext -> Time -> IO ()
loop context n = let a1 = mkActive 0 1 $ opaqueText context
                     a2 = mkActive 0 1 $ rotateSquare context
                     x  = movie [a2,a1]
                 in loopWorker x n

loopWorker :: Active (IO ()) -> Time -> IO ()
loopWorker x n = do
    runActive x n
    threadDelay (75 * 1000)
    loopWorker x $ mod' (n + 0.01) 2

opaqueText :: DeviceContext -> Time -> IO ()
opaqueText context n = send context $ do
    let (w, h) = (width context, height context)
    clearRect (0, 0, w, h)
    let n' = if n <= 0.5 then n else 1 - n  -- Logic for fade in and fade out
    globalAlpha $ fromTime n'
    font "40pt Calibri"
    fillText ("Opacity", 50, 50)

rotateSquare :: DeviceContext -> Time -> IO ()
rotateSquare context n = send context $ do
    let (w,h) = (width context, height context)
    clearRect (0, 0, w, h)
    beginPath ()
    save ()
    translate (w / 2, h / 2)
    rotate $ pi * (fromTime n)
    beginPath ()
    moveTo (-100, -100)
    lineTo (-100, 100)
    lineTo (100, 100)
    lineTo (100, -100)
    closePath ()
    lineWidth 10
    strokeStyle "green"
    stroke ()
    restore ()