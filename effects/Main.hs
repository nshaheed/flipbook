{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Data.Active
import Data.Fixed
import Data.Functor ((<$>))
import Data.Semigroup
import qualified Data.Text as Text
import Graphics.Blank

-- import Control.Monad.IO.Class
import Control.Monad.Trans

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
       loop context (0.5 :: Duration)

loop :: DeviceContext -> Duration -> IO ()
loop context d = do
          let a = clrScreen context
          let x0 = drawText context "40pt Calibri" "Text" (50,50)
              x1 = fadein context d x0
              x2 = waitAni x1 d
              x3 = translateAni context d (0,0) (0,400) x0
              x  = movie [x1,x2,x3]
          let y = fadein context d $ drawText context "40pt Calibri" "Text" (100,100)
          let x' = translateAni context d (50,50) (300,300) x0
          let z = [a,x]
          print . activeEra $ x
          -- send context $ drawText context
          -- send context $ drawText' context
          loopWorker z 0

loopWorker :: [Active (IO ())] -> Time -> IO ()
loopWorker xs n = do runAll xs n
                     threadDelay (75 * 1000)
                     print $ fromTime n
                     loopWorker xs (n + 0.01) -- $ mod' (n + 0.01) 2

loopWorker' :: Active (IO ()) -> Active (IO ())-> Time -> IO ()
loopWorker' xs ys n = do runAll' (xs,ys) n
                         threadDelay (75 * 1000)
                         print $ fromTime n
                         loopWorker' xs ys (n + 0.01) -- $ mod' (n + 0.01) 2                     
                      
                                                      
fadein, fadeout :: DeviceContext -> Duration -> Canvas () -> Active (IO ())
fadein context d c = clamp $ stretchTo d $ mkActive 0 1 $ \t -> send context $ do save()
                                                                                  globalAlpha(fromTime t)
                                                                                  c
                                                                                  restore()
fadeout context d c = backwards $ fadein context d c

--Animated Translate                          Start             End
translateAni :: DeviceContext -> Duration -> (Float, Float) -> (Float, Float) -> Canvas () -> Active (IO ())
translateAni context d (x1,y1) (x2,y2) c = clamp $ stretchTo d $
                                           mkActive 0 1 $ \t -> send context $ do save()
                                                                                  let (dx,dy) = (x2 - x1, y2 - y1)
                                                                                      t'      = fromTime t
                                                                                  translate (t' * dx + x1, t' * dy + y1)
                                                                                  c
                                                                                  restore()


-- movie' :: [Active a] -> Active a
-- movie' = foldr1 movie''

-- movie'' :: Active a -> Active a -> Active a
-- movie'' x y = (fromJust . getFirst) <$> ((First . Just <$> x) ->> (First . Just <$> y)) --TODO: Figure out what a semigroup is

runAll :: Monad m => [Active (m ())] -> Time -> m ()
runAll xs t = mapM_ (flip runActive t) xs

runAll' :: Monad m => (Active (m ()),Active (m())) -> Time -> m ()
runAll' (x,y) t = do runActive x t
                     runActive y t

-- runAllWorker :: [Active (IO ())]-> Time -> IO ()
-- runAllWorker xs t = mapM_ (flip runActive t) xs

clrScreen :: DeviceContext -> Active (IO ())
clrScreen context = mkActive 0 1 $ \t -> send context $ do clearRect (0,0, width context, height context)

drawText :: DeviceContext -> Text.Text -> Text.Text -> (Float,Float) -> Canvas ()
drawText context style text (x,y)= do font style
                                      fillText(text,50,50)

doNothing :: Duration -> Active (IO())
doNothing d = stretchTo d $ mkActive 0 1 $ \t -> do return ()

-- Maintain end of given active value for given duration
waitAni :: Active (IO()) -> Duration -> Active (IO())
waitAni a d = stretchTo d $ mkActive 0 1 $ \t -> activeEnd a
