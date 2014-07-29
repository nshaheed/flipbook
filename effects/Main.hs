{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Active
import Data.Fixed
import Graphics.Blank
import Control.Concurrent

-- import Control.Monad.IO.Class
import Control.Monad.Trans

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
       loop context (0.5 :: Duration)

loop :: DeviceContext -> Duration -> IO ()
loop context d = do
          let x = fadein context d $ drawText context
          let y = fadeout context d $ drawText context
          let z = movie [x]
          loopWorker y 0

loopWorker :: Active (IO ()) -> Time -> IO ()
loopWorker x n = do runActive x n
                    threadDelay (75 * 1000)
                    print $ fromTime n
                    loopWorker x 0.51--(n + 0.01) -- $ mod' (n + 0.01) 2

                                                      
fadein, fadeout :: DeviceContext -> Duration -> Canvas () -> Active (IO ())
fadein context d c = stretchTo d $ mkActive 0 1 $ \t' -> send context $ do save()
                                                                           globalAlpha(fromTime t')
                                                                           c
                                                                           restore()
fadeout context d c = backwards $ fadein context d c


drawText context = do let (w,h) = (width context, height context)
                      clearRect (0,0,w,h)
                      font "40pt Calibri"
                      fillText("Opacity",50,50)
