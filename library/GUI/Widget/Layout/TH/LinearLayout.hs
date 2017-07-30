{-# LANGUAGE CPP #-}
-- #define DEBUG_OUT
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:      GUI.Widget.Layout.TH.LinearLayout
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Линейные layout-ы бывают горизонтальные hLayout и вертикальные vLayout, но оба определяются
-- через одну slide (TH) функцию
-- т.к. имеют идентичную реализацию отличающуюся переменой координат осей.

module GUI.Widget.Layout.TH.LinearLayout where

import Language.Haskell.TH
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad.Extra (whenJust)
import Data.Maybe
import Data.Char
import qualified SDL
import SDL.Vect
import GUI
import GUI.Widget.TH
import GUI.Widget.Handlers
import GUI.Widget.Layout.LinearLayoutUtils

-- | Splice функция для создания функций горизонтального hLayout или вертикального vLayout
-- лайаута в модуле "GUI.Widget.Layout.LinearLayout".
mkLinearLayoutQ :: DirectionVH -> -- ^ Направление лайаута. Так же определяет первую букву генерируемой функции.
                   DecsQ
mkLinearLayoutQ direction = do
    argNameLst{-@[layoutDefN,parentN,skinN]-} <- mapM newName ["layoutDef","parent","skin"]
    let ~[layoutDef,parent,skin] = map varE argNameLst
        dirL = [directionLetter direction]
        ParallelOrthogonallyExpQFns{..} = getParallelOrthogonallyExpQFns direction
        body = normalB [|
            do
                rfSpaces <- newMonadIORef VU.empty
                let LayoutDef{..} = $layoutDef
                    color = fromMaybe (decoreBkColor (formDecore $skin)) layoutColor
                    arrangeChildren widget = do
                        (SDL.Rectangle p0 sz) <- getWidgetCanvasRect widget
                        szS <- readMonadIORef rfSpaces
                        let parallelAlign = $parallelGetAlign layoutAlignment
                            orthoAlign = $orthoGetAlign layoutAlignment
                            (V2 parellel0 ortho0) = $parallelV2 $ unP p0
                            (V2 parellelSz orthoSz) = $parallelV2 sz
                            (ls,rest) = linearLayoutCalc (VU.map $parallelGetFst szS) parellelSz
                            off = $parallelGetAlignOff parallelAlign rest
#ifdef DEBUG_OUT
                        sDbg1 <- widgetCoordsToStr widget
                        liftIO $ putStrLn $ concat ["arrangeChildren ", dirL, "  ",sDbg1, "  ls=",show ls,
                            "  rest=", show rest]
#endif
                        ifoldByWidgetChildren'_ (\coord i child -> do
                            let space = ls VU.! i
                                orthoChld = $orthoGetFst $ szS VU.! i
                                (ortCoord,ortSpace) = if orthoChld <0 then (ortho0,orthoSz)
                                                      else (ortho0 + $orthoGetAlignOff orthoAlign (orthoSz-orthoChld),
                                                            orthoChld)
                                r = $parallelRect $ SDL.Rectangle (P (V2 coord ortCoord)) (V2 space ortSpace)
#ifdef DEBUG_OUT
                            liftIO $ putStrLn $ concat ["arrangeChildren", dirL, ".byChildren  i=",show i,
                                "  coord=", show coord, "  space=", show space,
                                                        "  orthoChld=", show orthoChld, " r=", rectToBriefStr r]
#endif
                            widgetResizingIfChanged child r
                            return $ coord + space
                                               ) (parellel0+off) widget
                    fs = colorRectFns zero color
                mkWidget WidgetVisible layoutMargin (LinearLayoutData rfSpaces) $parent $ fs{
                    onSizeChangedParentNotify= \widget child sz -> do
                        mbI <- getChildWidgetIx widget child
#ifdef DEBUG_OUT
                        when (isNothing mbI) $ liftIO $ putStrLn $ concat
                            ["Layout ", dirL, ".onSizeChangedParentNotify  : mbI==Nothing  sz=",show sz]
#endif
                        whenJust mbI $ \i -> do
                            spaces <- readMonadIORef rfSpaces
#ifdef DEBUG_OUT
                            sDbgWidg <- widgetCoordsToStr widget
                            sDbgChld <- widgetCoordsToStr child
                            liftIO $ putStrLn $ concat ["Layout ", dirL, ".onSizeChangedParentNotify  ",sDbgWidg,
                                       "  CHILD: ",sDbgChld,"  i=",show i,"  sz=",show sz,"  spaces=", show spaces]
#endif
                            when (sz /= spaces VU.! i) $ do
                                    let spaces' = VU.modify (\v -> VUM.write v i sz) spaces
                                        maxOrtho = maxSignedCoordFromVector $ VU.map $orthoGetFst spaces'
                                    writeMonadIORef rfSpaces spaces'
#ifdef DEBUG_OUT
                                    liftIO $ putStrLn $ concat ["Layout ", dirL, ".onSizeChangedParentNotify  spaces'=",
                                        show spaces', "  maxOrtho=", show maxOrtho]
#endif
                                    notifyParentAboutSize widget $ $parallelMkV2 (-1) maxOrtho
#ifdef DEBUG_OUT
                                    liftIO $ putStrLn $ concat ["Layout ", dirL,
                                        ".onSizeChangedParentNotify  after notifyParentAboutSize"]
#endif
                                    arrangeChildren widget
                    ,onResizing= \widget newRect -> do
#ifdef DEBUG_OUT
                        sDbgWidg <- widgetCoordsToStr widget
                        liftIO $ putStrLn $ concat ["Layout ", dirL,
                                        ".onResizing  ", sDbgWidg, "   newRect=", rectToBriefStr newRect]
#endif
                        void $ simpleOnResizing widget newRect
#ifdef DEBUG_OUT
                        sDbgWidg2 <- widgetCoordsToStr widget
                        liftIO $ putStrLn $ concat ["Layout ", dirL,
                                        ".onResizing  after simpleOnResizing  ", sDbgWidg2]
#endif
                        arrangeChildren widget
                       } |]
        funName = mkName $ toLower (head dirL):"Layout"
    mN <- newName "m"
    let m = varT mN
    sequence [sigD funName [t| MonadIO $m => LayoutDef -> Widget -> Skin -> $m (GuiWidget LinearLayoutData) |]
             ,funD funName [clause (map varP argNameLst) -- [varP layoutDefN,wildP,varP skinN]
                body []]
             ]
