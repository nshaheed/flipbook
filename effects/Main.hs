{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text
import Effects
import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
  send context $ do translate(50,50)
                    (x,y) <- combineDraw "fesfes" "fesfs"
                    restore()

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
combineDraw :: Text.Text -> Text.Text -> Canvas (Float,Float)
combineDraw x y = do font "20pt Calibri"
                     TextMetrics wx <- measureText x
                     TextMetrics wy <- measureText y
                     let a = drawAfter wx y

                     fillText(x,0,0)
                     drawAfter wx y
                     return (wx,wx + wy)


drawAfter :: Float -> Text.Text -> Canvas ()
drawAfter x t = do save()
                   translate(x,0)
                   fillText(t,0,0)
                   restore()
                  
