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
                     x3 = waitAni x2 d'
                     x4'= offsetText arg xt textStyle (50,50) --drawText context textStyle xt (50,50)
                     x4 = waitAniCanv context d x4'
                     x5 = transText context d arg (50,50) in' (50,100) xt textStyle
                     x6 = waitAni x5 d
                     x7 = doNothing d
                     x' = atTime (toTime . fromDuration $ d) $ waitAniCanv context (d + d') $ combineDraw arg "" (50,50)
                     x''= movie [doNothing d, x']

                     -- x5 = waitAniCanv context d $ drawNonFunc xf (50,150) 

                     x = movie [x1,x2,x3,x4,x5,x6,x7]
                 let y0 = combineDraw in' f' (50,100)
                     y1 = fadein context d y0
                     y2 = waitAni y1 d'
                     y3 = y2
                     yf = match fName $ separate (Text.concat [in',f']) fName
                     y4 = waitAniCanv context d (drawNonFunc yf (50,100))
                     
                     yf'= replaceOnce "" xt yf 
                     ys@(ys1,ys2,ys3) = fuse yf'
                     yfinal = Text.concat [ys1,ys2,ys3]

                     y5 = offsetTrans context d ys fName textStyle (50,100)
                     y6 = waitAni y5 d
                     y7 = waitAniCanv context d $ drawText context textStyle yfinal (50,100)
                     y  = movie [y1,y2,y3,y4,y5,y6,y7]
                 let z1 = doNothing d
                     z2 = doNothing d'
                     z3 = parenFade context d' arg f (50,50)
                     z4 = fadeout context d $ drawFunc yf (50,100)
                     
                     z = movie [z1,z2,z3,z4]
                     
                 -- let y = fadein context d $ drawText context "40pt Courier" "Text" (100,100)
                 -- let x' = translateAni context d (50,50) (300,300) x0
                 let fullAni = [a,x,x'',y,z]
                 -- let fullAni = [a,x,y]
                 -- print . activeEra $ x
                 -- print yf
                 -- print yf'
                 -- print yfinal

                 loopWorker fullAni 0

loopWorker :: [Active (IO ())] -> Time -> IO ()                                                                                          
loopWorker xs n = do runAll xs n                                                                                                         
                     threadDelay (150 * 1000) --(75 * 1000)                                                                                                     -- print $ fromTime n   
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


replaceOnce :: Text.Text -> Text.Text -> [(Text.Text,Bool)] -> [(Text.Text,Bool)]
replaceOnce pre rep xs = (pre,False): replaceOnceWorker rep xs

replaceOnceWorker :: Text.Text -> [(Text.Text,Bool)] -> [(Text.Text,Bool)]
replaceOnceWorker _ []            = []
replaceOnceWorker rep ((t,b):tbs)
  | b         = [(rep,True),(Text.concat $ fst . unzip $ tbs, False)]
  | otherwise = (t,False): replaceOnceWorker rep tbs

fuse :: [(Text.Text,Bool)] -> (Text.Text,Text.Text,Text.Text)
fuse xs = let a = stopTrue xs
              b = findTrue xs
              c = afterTrue xs False
          in (a,b,c)

findTrue :: [(Text.Text,Bool)] -> Text.Text
findTrue [] = ""
findTrue ((t,b):tbs) = if b then t else findTrue tbs

stopTrue :: [(Text.Text,Bool)] -> Text.Text
stopTrue []          = ""
stopTrue ((t,b):tbs)
  | b         = ""
  | otherwise = Text.concat [t, stopTrue tbs]

afterTrue :: [(Text.Text,Bool)] -> Bool -> Text.Text
afterTrue [] _= ""
afterTrue ((t,b):tbs) past
  | past      = Text.concat [t, afterTrue tbs True]
  | otherwise = if b then afterTrue tbs True
                     else afterTrue tbs False

offsetTrans :: DeviceContext -> Duration -> (Text.Text,Text.Text,Text.Text) -> Text.Text -> Text.Text -> (Float,Float) -> Active(IO())
offsetTrans context d (t1,t2,t3) f style (x,y) = let c = do save()
                                                            font style
                                                            TextMetrics t1' <- measureText t1
                                                            TextMetrics f'  <- measureText f
                                                            fillText(t1,x,y)
                                                            restore()
                                                 in clamp $ stretchTo d $
                                                    mkActive 0 1 $ \t -> send context $ do save()
                                                                                           font style
                                                                                           c
                                                                                           TextMetrics t1' <- measureText t1
                                                                                           TextMetrics t2' <- measureText t2
                                                                                           TextMetrics f'  <- measureText f
                                                                                           translateAniCanv (x+t1'+f',y) (x+t1'+t2',y) t $ fillText(t3,0,0)
                                                                                           restore()

offsetText :: Text.Text -> Text.Text -> Text.Text -> (Float,Float) -> Canvas()
offsetText pre txt style (x,y) = do save()
                                    font style
                                    TextMetrics pre' <- measureText pre
                                    fillText(txt,x+pre',y)
                                    restore()
                                    
transText :: DeviceContext -> Duration -> Text.Text -> (Float,Float) -> Text.Text -> (Float,Float) -> Text.Text -> Text.Text -> Active(IO())
transText context d t1 (x1,y1) t2 (x2,y2) t0 style = clamp $ stretchTo d $ mkActive 0 1 $
                                                     \t -> send context $
                                                           do save()
                                                              font style
                                                              TextMetrics t1' <- measureText t1
                                                              TextMetrics t2' <- measureText t2
                                                              let x1' = x1 + t1'
                                                                  x2' = x2 + t2'
                                                              translateAniCanv (x1',y1) (x2',y2) t $ fillText(t0,0,0)
                                                              restore()

                                                           -- let c = save()
                                                           --   font style
                                                           --   TextMetrics t1' <- measureText t1
                                                           --   TextMetrics t2' <- measureText t2
                                                             
-- canvSections :: Text.Text -> (Float,Float) -> [(Text.Text,Bool)] -> [(Canvas(),Bool)]
-- canvSections style (x,y) ((t,b):tbs) = let canv = do save()
--                                                      font style
--                                                      TextMetrics t' <- measureText t
--                                                      when b $ fillText(t,x + t',y)
                                                    

                                              

-- translateAniText :: DeviceContext -> Duration -> 

-- topFuncAni :: DeviceContext -> Duration -> Text.Text -> Text.Text -> (Float,Float) -> Canvas ()
-- topFuncAni context d arg f (x,y) = TextMetrics arg' <- measureText arg
--                                    TextMetrics par <- measureText "("

-- strip :: Canvas TextMetrics -> Float
-- strip (TextMetrics x) = x
