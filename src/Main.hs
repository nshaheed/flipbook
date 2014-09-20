{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad

import           Data.Active
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Text (Text)

import           Graphics.Blank
import           Graphics.Flipbook.Effects

main :: IO ()
main = blankCanvas 3000 $ flip setup 0.5

setup :: DeviceContext -> Duration -> IO ()
setup context d = do
    let a = clrScreen context
        -- Functions to be displayed
        fName = "f" -- Text.pack "f"
        f     = Text.pack "\\x -> x + 2"
        arg   = Text.unwords ["let",fName,"=",""]
        in'   = "in "
        f'    = "f 3 + 2"
        d'    = d/6
        
        x1' = combineDraw arg f (50,50)
        x1  = fadein d x1'
        xt  = Text.append (Text.append "(" f) ")"
        x2  = translateAniTxt d' (50,50) f "(" textStyle arg
        x3  = waitAni x2 d'
        x4' = offsetText arg xt textStyle (50,50) -- drawText context textStyle xt (50,50)
        x4  = waitAniCanv d x4'
        x5  = transText d arg (50,50) in' (50,100) xt textStyle
        x6  = waitAni x5 d
        x7  = doNothing d
        dfrac = fromDuration d :: Float
        x'  = atTime (toTime dfrac) $ waitAniCanv (d + d') $ combineDraw arg "" (50,50)
        x'' = movie [doNothing d, x']
        -- x5 = waitAniCanv context d $ drawNonFunc xf (50,150)
        x   = movie [x1,x2,x3,x4,x5,x6,x7]
        
        y1' = combineDraw in' f' (50,100)
        y1  = fadein d y1'
        y2  = waitAni y1 d'
        y3  = y2
        yf  = match fName $ separate (Text.concat [in',f']) fName
        y4  = waitAniCanv d (drawNonFunc yf (50,100))

        yf'              = replaceOnce "" xt yf
        ys@(ys1,ys2,ys3) = fuse yf'
        yfinal           = Text.concat [ys1,ys2,ys3]

        y5 = offsetTrans d ys fName textStyle (50,100)
        y6 = waitAni y5 d
        y7 = waitAniCanv d $ drawText textStyle yfinal (50,100)
        y  = movie [y1,y2,y3,y4,y5,y6,y7]
        
        z1 = doNothing d
        z2 = doNothing d'
        z3 = parenFade d' arg f (50,50)
        z4 = fadeout d $ drawFunc yf (50,100)

        z = movie [z1,z2,z3,z4]

        -- fullAniRender = [x,x'',y,z]
        fullAni       = [a,x,x'',y,z]
        
    -- render fullAniRender context
    loop fullAni 0 context
    -- send context $ do fillText("fjdsla",0,0)

loop :: [Active (Canvas ())] -> Time -> DeviceContext -> IO ()
loop xs n context = do
    send context $ runAll xs n
    threadDelay (150 * 1000) -- (75 * 1000)
    loop xs (n + 0.01) context

render :: [Active (Canvas ())] -> DeviceContext -> IO ()
render xs context = let xss = rearrange $ map (simulate 200) xs
                        ys  = map combine' xss
                    in renderWorker ys context
                    -- in send context (combine' $ xss !! 0)
                    -- in send context $ do fillText("fjdklsaf",50,50)

renderWorker :: [Canvas ()] -> DeviceContext -> IO ()
renderWorker []     _       = return ()
renderWorker (x:xs) context = do
    send context $ do
        clearRect (0,0, width context, height context)
        x
    threadDelay (75 * 1000)
    renderWorker xs context

-- | skim top of list of lists
skim :: [[a]] -> [a]
skim []  = []
skim xss
  | hasEmpty xss = []
  | otherwise    = map head xss

remains :: [[a]] -> [[a]]
remains []  = []
remains xss
  | hasEmpty xss = []
  | otherwise    = map tail xss

hasEmpty :: [[a]] -> Bool
hasEmpty []     = False
hasEmpty (x:xs) = if null x then True else hasEmpty xs

rearrange :: [[a]] -> [[a]]
rearrange []  = []
rearrange xss = skim xss : (rearrange . remains $ xss)

combine' :: [Canvas ()] -> Canvas ()
combine' []     = return ()
combine' (x:xs) = x >> combine' xs
                    
-- | splits the first Text argument into sections delineated by
--   the second Text argument, with the second argument's value
--   included in the list          
separate :: Text -> Text -> [Text]
separate l s = let (x,y) = Text.breakOn s l
                   z = Text.stripPrefix s y
               in if isNothing z then [l]
                  else (x:s:separate (fromJust z) s)

-- | Takes two Text strings and places y immediately after x.
--   returns the width of the first string and the width of both
--   strings combined.
combineDraw :: Text -> Text -> (Double, Double) -> Canvas ()
combineDraw a b (x,y) = saveRestore $ do
    font textStyle
    TextMetrics wa <- measureText a
    TextMetrics _  <- measureText b
    translate (x, y)
    fillText (a, 0, 0)
    fillText (b, wa, 0)

drawAfter :: (Double, Double) -> Text -> Canvas ()
drawAfter (x, y) t = fillText (t, x, y)

textStyle :: Text.Text
textStyle = "20pt Calibri"

parenFade :: Duration -> Text -> Text -> (Double, Double) -> Active (Canvas())
parenFade d t1 t2 (x,y) = fadein d $ do
    save ()
    font textStyle
    TextMetrics t1' <- measureText t1
    TextMetrics t2' <- measureText t2
    TextMetrics p'  <- measureText "("
    drawText textStyle "(" (x+t1', y)
    drawText textStyle ")" (x+t1'+p'+t2', y)
    restore ()

match :: Eq a => a -> [a] -> [(a,Bool)]
match x ys = [(y, y == x) | y <- ys]

drawNonFunc :: [(Text, Bool)] -> (Double, Double) -> Canvas ()
drawNonFunc []          _      = return ()
drawNonFunc ((t, b):xs) (x, y) = do
    save ()
    font textStyle
    TextMetrics t' <- measureText t
    unless b $ fillText (t, x, y)
    restore ()
    drawNonFunc xs (x+t', y)

drawFunc :: [(Text, Bool)] -> (Double, Double) -> Canvas ()
drawFunc []          _      = return ()
drawFunc ((t, b):xs) (x, y) = do
    save ()
    font textStyle
    TextMetrics t' <- measureText t
    when b $ fillText (t, x, y)
    restore ()
    drawFunc xs (x+t', y)

replaceOnce :: Text -> Text -> [(Text, Bool)] -> [(Text, Bool)]
replaceOnce pre rep xs = (pre, False):replaceOnceWorker rep xs

replaceOnceWorker :: Text -> [(Text, Bool)] -> [(Text, Bool)]
replaceOnceWorker _   []           = []
replaceOnceWorker rep ((t, b):tbs)
  | b         = [(rep, True), (Text.concat $ fst . unzip $ tbs, False)]
  | otherwise = (t, False):replaceOnceWorker rep tbs

fuse :: [(Text, Bool)] -> (Text, Text, Text)
fuse xs = let a = stopTrue xs
              b = findTrue xs
              c = afterTrue xs False
          in (a, b, c)

findTrue :: [(Text, Bool)] -> Text
findTrue []           = ""
findTrue ((t, b):tbs) = if b then t else findTrue tbs

stopTrue :: [(Text, Bool)] -> Text
stopTrue []           = ""
stopTrue ((t, b):tbs)
  | b         = ""
  | otherwise = Text.concat [t, stopTrue tbs]

afterTrue :: [(Text, Bool)] -> Bool -> Text
afterTrue [] _= ""
afterTrue ((t,b):tbs) past
  | past      = Text.concat [t, afterTrue tbs True]
  | otherwise = if b then afterTrue tbs True
                     else afterTrue tbs False

offsetTrans :: Duration -> (Text, Text, Text) -> Text -> Text -> (Double, Double) -> Active (Canvas ())
offsetTrans d (t1,t2,t3) f style (x,y) =
  let c = do save ()
             font style
             TextMetrics _ <- measureText t1
             TextMetrics _ <- measureText f
             fillText (t1, x, y)
             restore ()
  in clamp . stretchTo d . mkActive 0 1 $ \t -> do
             save()
             font style
             c
             TextMetrics t1' <- measureText t1
             TextMetrics t2' <- measureText t2
             TextMetrics f'  <- measureText f
             translateAniCanv (x+t1'+f', y) (x+t1'+t2', y) t $ fillText (t3, 0, 0)
             restore()
                                                                    
offsetText :: Text -> Text -> Text -> (Double, Double) -> Canvas ()
offsetText pre txt style (x, y) = do
    save ()
    font style
    TextMetrics pre' <- measureText pre
    fillText (txt,x+pre', y)
    restore ()

transText :: Duration -> Text -> (Double, Double) -> Text -> (Double, Double) -> Text -> Text -> Active (Canvas ())
transText d t1 (x1, y1) t2 (x2, y2) t0 style = clamp . stretchTo d . mkActive 0 1 $ \t -> do
    save ()
    font style
    TextMetrics t1' <- measureText t1
    TextMetrics t2' <- measureText t2
    let x1' = x1 + t1'
        x2' = x2 + t2'
    translateAniCanv (x1', y1) (x2', y2) t $ fillText (t0, 0, 0)
    restore ()