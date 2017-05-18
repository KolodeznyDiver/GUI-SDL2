{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module GUI.Widget.Layout.TH.LinearLayout where

import Language.Haskell.TH
import Control.Monad
import Control.Monad.IO.Class
--import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
--import MonadUtils (whenM)
import Maybes (whenIsJust)
import Data.Maybe
import Data.Char
import qualified SDL
import SDL.Vect
import GUI
import GUI.Widget.TH
import GUI.Widget.Handlers
--import Data.Vector.Utils
import GUI.Widget.Layout.Utils

mkLinearLayoutQ :: DirectionVH -> DecsQ
mkLinearLayoutQ direction = do
    argNameLst{-@[layoutDefN,parentN,skinN]-} <- mapM newName ["layoutDef","parent","skin"]
    let ~[layoutDef,parent,skin] = map varE argNameLst
        dirL = [directionLetter direction]
        ParallelOrthogonallyExpQFns{..} = getParallelOrthogonallyExpQFns direction
        body = normalB [|
            do
                rfSpaces <- newMonadIORef VU.empty
                let LayoutDef{..} = $layoutDef
                    color = fromMaybe (bkColor $skin) layoutColor
                    arrangeChildren widget = do
                        (SDL.Rectangle p0 sz) <- getWidgetCanvasRect widget
                        szS <- readMonadIORef rfSpaces
                        let parallelAlign = $parallelGetAlign layoutAlignment
                            orthoAlign = $orthoGetAlign layoutAlignment
                            (V2 parellel0 ortho0) = $parallelV2 $ unP p0
                            (V2 parellelSz orthoSz) = $parallelV2 sz
                            (ls,rest) = linearLayoutCalc (VU.map $parallelGetFst szS) parellelSz
                            off = $parallelGetAlignOff parallelAlign rest
--                        sDbg1 <- widgetCoordsToStr widget
--                        liftIO $ putStrLn $ concat ["arrangeChildren ", dirL, "  ",sDbg1, "  ls=",show ls,
--                            "  rest=", show rest]
                        ifoldByWidgetChildren'_ (\coord i child -> do
                            let space = ls VU.! i
                                orthoChld = $orthoGetFst $ szS VU.! i
                                (ortCoord,ortSpace) = if orthoChld <0 then (ortho0,orthoSz)
                                                      else (ortho0 + $orthoGetAlignOff orthoAlign (orthoSz-orthoChld),
                                                            orthoChld)
                                r = $parallelRect $ SDL.Rectangle (P (V2 coord ortCoord)) (V2 space ortSpace)
--                            liftIO $ putStrLn $ concat ["arrangeChildren", dirL, ".byChildren  i=",show i,
--                                "  coord=", show coord, "  space=", show space,
--                                                        "  orthoChld=", show orthoChld, " r=", rectToBriefStr r]
                            widgetResizingIfChanged child r
                            return $ coord + space
                                               ) (parellel0+off) widget
                    fs = colorRectFns zero color
                mkWidget WidgetVisible layoutMargin (LinearLayoutData rfSpaces) $parent $ fs{
                    onSizeChangedParentNotiy= \widget child sz -> do
                        mbI <- getChildWidgetIx widget child
                        when (mbI==Nothing) $ liftIO $ putStrLn $ concat
                            ["Utils ", dirL, ".onSizeChangedParentNotiy  : mbI==Nothing  sz=",show sz]
                        whenIsJust mbI $ \i -> do
                            spaces <- readMonadIORef rfSpaces
--                            sDbgWidg <- widgetCoordsToStr widget
--                            sDbgChld <- widgetCoordsToStr child
--                            liftIO $ putStrLn $ concat ["Utils ", dirL, ".onSizeChangedParentNotiy  ",sDbgWidg,
--                                       "  CHILD: ",sDbgChld,"  i=",show i,"  sz=",show sz,"  spaces=", show spaces]
                            when (sz /= spaces VU.! i) $ do
                                    let spaces' = VU.modify (\v -> VUM.write v i sz) spaces
                                        maxOrtho = maxSignedCoordFromVector $ VU.map $orthoGetFst spaces'
                                    writeMonadIORef rfSpaces spaces'
--                                    liftIO $ putStrLn $ concat ["Utils ", dirL, ".onSizeChangedParentNotiy  spaces'=",
--                                        show spaces', "  maxOrtho=", show maxOrtho]
                                    notifyParentSizeWithMargin widget $ $parallelMkV2 (-1) maxOrtho
{-                                    liftIO $ putStrLn $ concat ["Utils ", dirL,
                                        ".onSizeChangedParentNotiy  after notifyParentSizeWithMargin"] -}
                                    arrangeChildren widget
                    ,onResizing= \widget newRect -> do
--                        sDbgWidg <- widgetCoordsToStr widget
--                        liftIO $ putStrLn $ concat ["Utils ", dirL,
--                                        ".onResizing  ", sDbgWidg, "   newRect=", rectToBriefStr newRect]
                        void $ simpleOnResizing widget newRect
--                        sDbgWidg2 <- widgetCoordsToStr widget
--                        liftIO $ putStrLn $ concat ["Utils ", dirL,
--                                        ".onResizing  after simpleOnResizing  ", sDbgWidg2]
                        arrangeChildren widget
                       } |]
        funName = mkName $ toLower (head dirL):"Layout"
    mN <- newName "m"
    let m = varT mN
    sequence [sigD funName [t| MonadIO $m => LayoutDef -> Widget -> Skin -> $m (GuiWidget LinearLayoutData) |]
             ,funD funName [clause (map varP argNameLst) -- [varP layoutDefN,wildP,varP skinN]
                body []]
             ]
