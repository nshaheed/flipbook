{-# LANGUAGE OverloadedStrings #-}
module Effects (
      fadein
    , fadeout
    , translateAni
    , translateAniTxt
    , translateAniCanv
    , runAll
    , clrScreen
    , drawText
    , doNothing
    , waitAni
    , aniActive1
    , aniActive2
    , waitAniCanv
    ) where

import Control.Applicative (pure)

import Data.Active
import Data.Text (Text)

import Graphics.Blank

-- | Shorthand for constructing active values needed for animations
aniActive1 :: Duration -> (Time -> a) -> Active a
aniActive1 d a = stretchTo d . mkActive 0 1 $ a

aniActive2 :: Duration -> (Time -> a) -> Active a
aniActive2 d a = clamp . stretchTo d $ mkActive 0 1 a

-- :: Duraction -> a -> Active a
fadein, fadeout :: Duration -> Canvas () -> Active (Canvas())
fadein d c = aniActive2 d $ \t -> saveRestore $ do
    globalAlpha $ fromTime t
    c
fadeout d c  = backwards $ fadein d c

-- | Animated Translation
translateAni :: Duration -> (Float, Float) -> (Float, Float) -> Canvas () -> Active (Canvas ())
-- translateAni d start' end' c = clamp $ stretchTo d $ mkActive 0 1 $ \t -> translateAniCanv start' end' t c
translateAni d start' end' c = aniActive2 d $ \t -> translateAniCanv start' end' t c

-- | Displaces a given text by the width of a given displacement text, requires the preceding text for width
translateAniTxt :: Duration -> (Float, Float) -> Text -> Text -> Text -> Text -> Active(Canvas ())
translateAniTxt d (x,y) txt disp precede style = aniActive2 d $ \t -> saveRestore $ do
    font style
    TextMetrics precede' <- measureText precede
    TextMetrics disp'    <- measureText disp
    let txt' = drawText style txt (0,0)
    translateAniCanv (x+precede', y) (x+precede'+disp', y) t txt'
-- translateAniTxt d (x,y) txt disp style initial = clamp $ stretchTo d $
--                                                  mkActive 0 1 $ \t -> saveRestore $ do font style
--                                                                                        TextMetrics initial' <- measureText initial
--                                                                                        TextMetrics disp' <- measureText disp
--                                                                                        let txt' = drawText style txt (0,0)
--                                                                                        translateAniCanv (x+initial',y) (x+initial'+disp',y) t txt'


-- | The Canvas backend of any translateAni functions
translateAniCanv :: (Float, Float) -> (Float, Float) -> Time -> Canvas () -> Canvas ()
translateAniCanv (x1, y1) (x2, y2) t c = saveRestore $ do
    let (dx,dy) = (x2 - x1, y2 - y1)
        t'      = fromTime t
    translate (t' * dx + x1, t' * dy + y1)
    c
                                                        
runAll :: Monad m => [Active (m ())] -> Time -> m ()
runAll xs t = combine xs t

combine :: Monad m => [Active (m ())]-> Time -> m ()
combine []     _ = return ()
combine (x:xs) t = runActive x t >> combine xs t

clrScreen :: DeviceContext -> Active (Canvas ())
clrScreen context = pure $ clearRect (0, 0, width context, height context)

drawText :: Text -> Text -> (Float, Float) -> Canvas ()
drawText style text (x,y)= do
    font style
    fillText (text, x, y)

-- | Returns a blank screen of duration d
doNothing :: Duration -> Active (Canvas ())
doNothing d = aniActive1 d . const $ return ()

-- | Maintain end of given active value for given duration
waitAni :: Active a -> Duration -> Active a
waitAni a d = aniActive1 d . const $ activeEnd a

waitAniCanv :: Duration -> Canvas () -> Active(Canvas ())
waitAniCanv d c = aniActive1 d $ const c