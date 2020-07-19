{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      GUI.Widget.TH.LinearTrackBar
-- Copyright:   (c) 2017-2020 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Генерация функций линейных трекбаров. Есть два более гибких в настройке :
-- Горизонтальный hLinearTrackBar и вертикальный vLinearTrackBar, но оба определяются через одну slide (TH)
-- функцию @mkLinearTrackBarQ@ т.к. имеют идентичную реализацию отличающуюся переменой координат осей.
-- А так же два полученных из них : горизонтальный hTrackBar и вертикальный vTrackBar, со встроенными функциями отрисовки,
-- см. "GUI.Widget.LinearTrackBar".

module GUI.Widget.TH.LinearTrackBar(
    mkLinearTrackBarQ
                                   ) where

import Language.Haskell.TH
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import qualified SDL
import SDL.Vect
import GUI
import GUI.Widget.TH
import GUI.Widget.Handlers
import GUI.Widget.Internal.LinearTrackBar

pattern SliderMinLn :: Coord
pattern SliderMinLn = 6

-- | Splice функция для создания функций горизонтального hLinearTrackBar или вертикального vLinearTrackBar
-- трекбара в модуле "GUI.Widget.LinearTrackBar".
mkLinearTrackBarQ :: DirectionVH -- ^ Направление трекбара. Так же определяет первую букву генерируемой функции.
                     -> DecsQ
mkLinearTrackBarQ direction = do
    argNameLst <- mapM newName ["initData","parent","skin"]
    mN <- newName "m"
    let ~[initData,parent,skin] = map varE argNameLst
        m = varT mN
        dirL = [directionLetter direction]
        ParallelOrthogonallyExpQFns{..} = getParallelOrthogonallyExpQFns direction
        body = normalB [|
            do
                let LinearTrackBarDef{..} = $initData
                    sliderNoTracing = (minBound,0) -- minBound - не происходит перемещение ползунка
                rfSliderTraceState <- newMonadIORef sliderNoTracing
                -- onChanged' <- newMonadIORef $ OneArgAction ( return . const ())
                dataRf <- newMonadIORef LinearTrackBarStruct  { lnrTrBrMin = linearTrackMinValue
                                                              , lnrTrBrMax = linearTrackMaxValue
                                                              , lnrTrBrVal = linearTrackBarPos
                                                              , lnrTrBrSliderLn = linearTrackBarSliderLn
                                                              , lnrTrBrOnChanged = \_ -> return ()
                                                              }
                let getSlideLn l trackLn = toBound SliderMinLn trackLn (
                        round $ lnrTrBrSliderLn l * fromIntegral trackLn
                        {-if linearTrackBarSliderLn > 0 then linearTrackBarSliderLn
                        else mulDiv trackLn trackLn (max 1 $ lnrTrBrMax l - lnrTrBrMin l)-}
                                                                                              )
                    getSlideFromTo l (SDL.Rectangle (P p0) sz) =
                        let track0 = $parallelGetFst p0
                            trackLn = $parallelGetFst sz
                            track1 = track0 + trackLn
                            slideLn = getSlideLn l trackLn
                            hf = slideLn `div` 2
                            result x = (max 0 $ x - hf, x + hf) in
                        if lnrTrBrMin l >= lnrTrBrMax l
                        then {- return $ -} result hf
                        else {- do
                                let res = -}
                                 result $ toBound (track0 + hf) (track1 - hf) $
                                        hf + mulDiv (trackLn - slideLn)
                                               (lnrTrBrVal l - lnrTrBrMin l)
                                               (lnrTrBrMax l - lnrTrBrMin l)
{-                                liftIO $ putStrLn $ concat ["getSlideFromTo  r=",rectToBriefStr r,
                                    "  track0=", show track0,
                                    "  trackLn=", show trackLn, "  slideLn=", show slideLn,
                                    "  hf=", show hf, "  lnrTrBrVal=", show $ lnrTrBrVal l,
                                    "  mulDiv args=", show (trackLn - slideLn),",",
                                    show (lnrTrBrVal l -lnrTrBrMin l),",",show (lnrTrBrMax l - lnrTrBrMin l),
                                    " mulDiv=", show (mulDiv (trackLn - slideLn) (lnrTrBrVal l -lnrTrBrMin l)
                                                     (lnrTrBrMax l - lnrTrBrMin l))]
                                return res  -}
                    getNewValByCoord l (SDL.Rectangle (P p0) sz) coord =
                        let coord0 =  $parallelGetFst p0
                            trackLn = $parallelGetFst sz
                            sliderLn = getSlideLn l trackLn in
                        toBound (lnrTrBrMin l) (lnrTrBrMax l) $ lnrTrBrMin l + mulDiv
                              (coord - coord0 - (sliderLn `div` 2))
                              (lnrTrBrMax l - lnrTrBrMin l) (max 1 $ trackLn - sliderLn)
                    getNewValByMouse l (SDL.Rectangle _ sz) deltaCoord oldV =
                        let trackLn = $parallelGetFst sz in
                        toBound (lnrTrBrMin l) (lnrTrBrMax l) $
                            oldV + mulDiv deltaCoord (lnrTrBrMax l - lnrTrBrMin l)
                                          (max 1 $ trackLn - getSlideLn l trackLn)
                    setNewVIfChange widget l newV =
                            when (newV /= lnrTrBrVal l) $ do
                                writeMonadIORef dataRf l{lnrTrBrVal=newV}
                                markWidgetForRedraw widget
                                lnrTrBrOnChanged l newV

                    setValByMouse widget (P p) = do
                        (oldC,oldV) <- readMonadIORef rfSliderTraceState
                        let  newC = $parallelGetFst p
                        when (oldC /= minBound && oldC /= newC) $ do
                            r <- getWidgetVisibleRect widget
                            l <- readMonadIORef dataRf
                            newV <- linearTrackBarRounder $ getNewValByMouse l r (newC - oldC) oldV
                            setNewVIfChange widget l newV
                    setSliderTraceState  :: MonadIO $m => (Coord,LinearTrackValueType) -> $m ()
                    setSliderTraceState = writeMonadIORef rfSliderTraceState
                    resetSliderTrace :: MonadIO $m => $m ()
                    resetSliderTrace = setSliderTraceState sliderNoTracing
                MouseAnimatedClickableHelper'
                        { mouseAnimatedClickableMouseState' = mouseState
                        , mouseAnimatedClickableFs' = fs
                        } <- mouseAnimatedClickableHelper'
                            ( $parallelMkV2 linearTrackBarLn (trackBarWidth $skin))
                            (\_ _ _ -> return () ) {- $ OneArgPredicate -} (\_ _ -> return True )
                mkFormWidget linearTrackBarFormItemDef linearTrackBarFlags $skin id
                                 (LinearTrackBarData dataRf) $parent $ fs{
                    onMouseMotion = \widget btnsLst p _ {-relMv-} ->
                        when (SDL.ButtonLeft `elem` btnsLst) $ setValByMouse widget p
                    ,onLostMouseFocus = \widget -> resetSliderTrace >> onLostMouseFocus fs widget
                    ,onMouseButton = \widget motion mouseButton _ {-clicks -} (P p) -> do
                        ena <- allWidgetFlags widget WidgetEnable
                        when (ena && (mouseButton == SDL.ButtonLeft)) (
                                if motion == SDL.Pressed then do
                                        r <- getWidgetVisibleRect widget
                                        d <- readMonadIORef dataRf
                                        let  newC = $parallelGetFst p
                                             (c0,c1) = getSlideFromTo d r
                                        if c0<newC && newC<c1 then do
                                            setSliderTraceState (newC,lnrTrBrVal d)
                                            writeMonadIORef mouseState WidgetMousePressed
                                            markWidgetForRedraw widget
                                        else do
                                            newV <- linearTrackBarRounder $ getNewValByCoord d r newC
                                            setNewVIfChange widget d newV
                                else do
                                        writeMonadIORef mouseState WidgetMouseIn
                                        resetSliderTrace
                                        markWidgetForRedraw widget
                            )
                    ,onDraw= \widget -> do
                        r <- getVisibleRect widget
                        ena <- allWidgetFlags widget WidgetEnable
                        ms  <- if ena then fmap Just (readMonadIORef mouseState)
                               else return Nothing
                        linearTrackBarDraw widget ms r
                        d <- readMonadIORef dataRf
--                        (c0,c1) <- getSlideFromTo d r
                        let (c0,c1) = getSlideFromTo d r
                            (SDL.Rectangle (P (V2 _ b)) (V2 _ h)) = $parallelRect r
                            slideR = $parallelRect (SDL.Rectangle (P (V2 c0 b)) (V2 (c1-c0) h))
                        linearTrackBarSliderDraw widget ms slideR
                       } |]
        funName = mkName $ toLower (head dirL):"LinearTrackBar"
    sequence [sigD funName [t| MonadIO $m => LinearTrackBarDef -> Widget -> Skin -> $m (GuiWidget LinearTrackBarData) |]
             ,funD funName [clause (map varP argNameLst) body []]
             ]
