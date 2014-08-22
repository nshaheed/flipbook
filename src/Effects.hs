{-# LANGUAGE OverloadedStrings #-}
module Effects
       (fadein,
        fadeout,
        translateAni,
        translateAniTxt,
        translateAniCanv,
        runAll,
        clrScreen,
        drawText,
        doNothing,
        waitAni,
        waitAniCanv) where

import Control.Applicative (pure)

import Data.Active
import Data.Text (Text)

import Graphics.Blank

fadein, fadeout :: Duration -> Canvas () -> Active (Canvas())
fadein d c = clamp $ stretchTo d $ mkActive 0 1 $ \t -> do save()
                                                           globalAlpha(fromTime t)
                                                           c
                                                           restore()
fadeout d c  = backwards $ fadein d c

--Animated Translate

translateAni :: Duration -> (Float, Float) -> (Float, Float) -> Canvas () -> Active(Canvas ())
translateAni d start' end' c = clamp $ stretchTo d $ mkActive 0 1 $ \t -> translateAniCanv start' end' t c

translateAniTxt :: Duration -> (Float, Float) -> Text -> Text -> Text -> Text -> Active(Canvas ())
translateAniTxt d (x,y) txt disp style initial = clamp $ stretchTo d $
                                                 mkActive 0 1 $ \t -> do save()
                                                                         font style
                                                                         TextMetrics initial' <- measureText initial
                                                                         TextMetrics disp' <- measureText disp
                                                                         let txt' = drawText style txt (0,0)
                                                                         translateAniCanv (x+initial',y) (x+initial'+disp',y) t txt'
                                                                         restore()

-- The Canvas backend of any translateAni functions
translateAniCanv :: (Float, Float) -> (Float, Float) -> Time -> Canvas () -> Canvas ()
translateAniCanv (x1,y1) (x2,y2) t c = do save()
                                          let (dx,dy) = (x2 - x1, y2 - y1)
                                              t'      = fromTime t
                                          translate (t' * dx + x1, t' * dy + y1)
                                          c
                                          restore()

runAll :: Monad m => [Active (m ())] -> Time -> m ()
runAll xs t = combine xs t

combine :: Monad m => [Active (m ())]-> Time -> m ()
combine [] _     = return ()
combine (x:xs) t = (runActive x t) >> (combine xs t)

clrScreen :: DeviceContext -> Active (Canvas ())
clrScreen context = pure $ do clearRect (0,0, width context, height context)

drawText :: Text -> Text -> (Float,Float) -> Canvas ()
drawText style text (x,y)= do font style
                              fillText(text,x,y)

doNothing :: Duration -> Active (Canvas ())
doNothing d = stretchTo d . mkActive 0 1 . const $ return ()                                                    

-- Maintain end of given active value for given duration
waitAni :: Active a -> Duration -> Active a
waitAni a d = stretchTo d . mkActive 0 1 . const $ activeEnd a

waitAniCanv :: Duration -> Canvas () -> Active(Canvas ())
waitAniCanv d c = stretchTo d . mkActive 0 1 $ const c
