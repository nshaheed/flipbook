{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent

import Data.Active
import Data.Fixed

import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 $ flip loop 0

loop :: DeviceContext -> Time -> IO ()
loop context n = let x = stretch 2 . mkActive 0 1 $ opaqueText context
                 in loopWorker x n

loopWorker :: Active (IO ()) -> Time -> IO ()
loopWorker x n = do
    runActive x n
    threadDelay (75 * 1000)
    loopWorker x $ mod' (n + 0.01) 2

opaqueText :: DeviceContext -> Time -> IO ()
opaqueText context n = send context $ do
    let (w,h) = (width context, height context)
    clearRect (0, 0, w, h)
    let n' = if n <= 1 then n else 2 - n  -- Logic for fade in and fade out
    globalAlpha $ fromTime n'
    font "40pt Calibri"
    fillText ("Opacity",50,50)