{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Active
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import Effects
import Graphics.Blank

-- main :: IO ()
-- main = blankCanvas 3000 $ \ context -> do
--   send context $ do translate(50,50)
--                     (x,y) <- combineDraw "fesfes" "fesfs"
--                     restore()

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
       loop context (0.5 :: Duration)

loop :: DeviceContext -> Duration -> IO ()
loop context d = do
                 let a = clrScreen context

                 -- Functions to be displayed
                 let fName = "f"--Text.pack "f"
                     f     = Text.pack "\\x -> x + 2"
                     arg   = Text.unwords ["let",fName,"=",""]
                     in'   = "in "
                     f'    = "f 3 + 2"
                     d'    = d/6
                 let x0 = combineDraw arg f (50,50)
                     x1 = fadein context d x0
                     xt = Text.append (Text.append "(" f) ")"
                     x2 = translateAniTxt context d' (50,50) f "(" textStyle arg
                     x3 = waitAni x2 d
                     x4 = waitAniCanv context d $ combineDraw arg xt (50,50)
                     x' = atTime (toTime . fromDuration $ d) $ waitAniCanv context (d + d') $ combineDraw arg "" (50,50)
                     x''= movie [doNothing d, x', doNothing d]
                     -- xf = match fName $ separate f' fName
                     -- x5 = waitAniCanv context d $ drawNonFunc xf (50,150) 

                     x = movie [x1,x2,x3,x4,x5]
                 let y0 = combineDraw in' f' (50,100)
                     y1 = fadein context d y0
                     y2 = waitAni y1 (d/5)
                     
                     y = movie [y1,y2]
                 let z1 = doNothing (d + d')
                     z2 = parenFade context d' arg f (50,50)
                     
                     z = movie [z1,z2]
                     
                 -- let y = fadein context d $ drawText context "40pt Courier" "Text" (100,100)
                 -- let x' = translateAni context d (50,50) (300,300) x0
                 let fullAni = [a,x,x'',y,z]
                 -- print . activeEra $ x
                 -- send context $ drawText context
                 -- send context $ drawText' context
                 loopWorker fullAni 0

loopWorker :: [Active (IO ())] -> Time -> IO ()                                                                                          
loopWorker xs n = do runAll xs n                                                                                                         
                     threadDelay (75 * 1000)                                                                                             
                     -- print $ fromTime n   
                     loopWorker xs (n + 0.01) -- $ mod' (n + 0.01) 2 


--splits the first Text argument into sections delineated by
--the second Text argument, with the second argument's value
--included in the list          
separate :: Text.Text -> Text.Text -> [Text.Text]
separate l s = let (x,y) = Text.breakOn s l
                   z = Text.stripPrefix s y
               in if isNothing z then [l]
                  else (x:s:separate (fromJust z) s)


--TextMetrics w <_ measureText text

--Takes two Text strings and places y immediately after x.
--returns the width of the first string and the width of both
--  strings combined.
combineDraw :: Text.Text -> Text.Text -> (Float,Float) -> Canvas () --(Float,Float)
combineDraw a b (x,y) = do save()
                           font textStyle
                           TextMetrics wa <- measureText a
                           TextMetrics wb <- measureText b
                           translate(x,y)
                           fillText(a,0,0)
                           drawAfter (wa,0) b
                           restore()
                           -- return (wa,wa + wb)


drawAfter :: (Float,Float) -> Text.Text -> Canvas ()
drawAfter (x,y) t = do save()
                       translate(x,y)
                       fillText(t,0,0)
                       restore()

textStyle :: Text.Text
textStyle = "20pt Calibri"

parenFade :: DeviceContext -> Duration -> Text.Text -> Text.Text -> (Float,Float) -> Active(IO())
parenFade context d t1 t2 (x,y) = fadein context d $ do save()
                                                        font textStyle
                                                        TextMetrics t1' <- measureText t1
                                                        TextMetrics t2' <- measureText t2
                                                        TextMetrics p'  <- measureText "("
                                                        drawText context textStyle "(" (x+t1',y)
                                                        drawText context textStyle ")" (x+t1'+p'+t2',y)
                                                        restore()

match :: Eq a => a -> [a] -> [(a,Bool)]
match x ys = [(y, y == x) | y <- ys]

drawNonFunc :: [(Text.Text,Bool)] -> (Float, Float) -> Canvas ()
drawNonFunc [] _             = return ()
drawNonFunc ((t,b):xs) (x,y) = do save()
                                  font textStyle
                                  TextMetrics t' <- measureText t
                                  when (not b) $ fillText(t,x,y)
                                  restore()
                                  drawNonFunc xs (x+t',y)


drawFunc :: [(Text.Text,Bool)] -> (Float, Float) -> Canvas ()
drawFunc [] _             = return ()
drawFunc ((t,b):xs) (x,y) = do save()
                               font textStyle
                               TextMetrics t' <- measureText t
                               when b $ fillText(t,x,y)
                               restore()
                               drawFunc xs (x+t',y)                                  
                                  
                                  
                                 

-- translateAniText :: DeviceContext -> Duration -> 

-- topFuncAni :: DeviceContext -> Duration -> Text.Text -> Text.Text -> (Float,Float) -> Canvas ()
-- topFuncAni context d arg f (x,y) = TextMetrics arg' <- measureText arg
--                                    TextMetrics par <- measureText "("

-- strip :: Canvas TextMetrics -> Float
-- strip (TextMetrics x) = x
