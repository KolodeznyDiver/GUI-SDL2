{-# LANGUAGE RecordWildCards #-}
module GUI.Utils.TextWrap(
    PreparedWrapText,prepareWrapText,getHeightOfPreparedWrapText,drawSplitPreparedText
    ,TextWrapMode(..),DrawTextDef(..),PreparedText(..),prepareText,drawPreparedText,textWrapModeToMbBkColor
                        ) where

import Control.Monad.IO.Class
--import Control.Monad.Trans.Class
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Maybe
import Data.Default
import qualified SDL
import SDL.Vect
import SDL.TTF.FFI (TTFFont)
import GUI
import qualified GUI.BaseLayer.Primitives as P
import GUI.Utils.Wrap

data PreparedWrapText = PreparedWrapText (V.Vector T.Text) (VU.Vector GuiPoint) Coord Coord
        deriving (Show, Eq)

prepareWrapText :: MonadIO m => Alignment -> GuiRect -> Double -> TTFFont -> T.Text -> m PreparedWrapText
prepareWrapText align r@(SDL.Rectangle (P (V2 x0 _)) (V2 width _)) lineSpacing fnt str =
    let wrds = V.fromList $ T.words str
        cWords = V.length wrds in
    if cWords == 0 then return $ PreparedWrapText V.empty VU.empty x0 0
    else do
        szS <- VU.generateM cWords (P.strSize fnt . T.unpack . (wrds V.!) )
        (V2 spaceWidth _) <- P.strSize fnt " "
--        liftIO $ putStrLn $ concat ["textSplitPrepare  width=", show width, "   szS=",show szS]
        let vv = rowWrapping (getHAlign align) width spaceWidth szS
            cElem = V.sum $ V.map (VU.length . fst) vv
--        liftIO $ putStrLn $ concat ["textSplitPrepare vv=", show vv, " lineSpacing=", show lineSpacing,
--           "  dY=", show (truncate ((1+lineSpacing) * (fromIntegral $ snd $ V.head vv))) ]
        let offsets = VU.create $ do
                        v <- VUM.new cElem
                        let go y i m | Just (vRow,maxH) <- vv V.!? i =
                                        let byRow j n | Just x <- vRow VU.!? j = do
                                                                VUM.write v n (V2 x y)
                                                                byRow (j+1) (n+1)
                                                      | otherwise = return n
                                        in byRow 0 m >>=
                                                go (y + ceiling (lineSpacing * fromIntegral maxH)) (i + 1)
                                     | otherwise = return v
                        go 0 0 0
            (V2 _ maxY) = VU.last offsets
            prepH = maxY + snd (V.last vv)
--            offY = vAlignToOff (getVAlign align) $ height - prepH
--            pLT = p0 .+^ (V2 0 offY)
--        liftIO $ putStrLn $ concat ["textSplitPrepare offsets=", show offsets]
        return $ moveWrapText (getVAlign align) r $ PreparedWrapText wrds (VU.map P offsets) 0 prepH

moveWrapText :: VAlign -> GuiRect -> PreparedWrapText -> PreparedWrapText
moveWrapText align (SDL.Rectangle (P (V2 newX newY)) (V2 _ height)) pwt@(PreparedWrapText vt vp oldX prepH)
    | VU.null vp = pwt
    | otherwise = let (P (V2 _ top)) = VU.head vp
                      off= V2 (newX - oldX) (newY + vAlignToOff align (height - prepH) - top) in
                  PreparedWrapText vt (VU.map ( .+^ off) vp) newX prepH

getHeightOfPreparedWrapText :: PreparedWrapText -> Coord
getHeightOfPreparedWrapText (PreparedWrapText _ _ _ h) = h
{-# INLINE getHeightOfPreparedWrapText #-}

drawSplitPreparedText :: MonadIO m => PreparedWrapText -> TTFFont -> GuiColor -> Maybe GuiColor -> Canvas m ()
drawSplitPreparedText (PreparedWrapText vT vP _ _) fnt color mbBkColor =
    let f = case mbBkColor of
                Just bc -> drawTextOpaque fnt color bc
                _ -> drawText fnt color
    in V.imapM_ (\i -> f (vP VU.! i)) vT
{-# INLINE drawSplitPreparedText #-}

data TextWrapMode = TextNoWrap {textAreaMaxWidth :: Coord}
                  | TextWrap {textAreaMaxHeight :: Coord, textAreaLineSpacing :: Maybe Double}
        deriving (Show, Eq)

instance Default TextWrapMode where
    def = TextNoWrap 0

data DrawTextDef = DrawTextDef  { drawTextRect :: GuiRect
                                , drawTextWrap :: TextWrapMode
                                , drawTextFontKey :: T.Text
                                , drawTextAlignment :: Alignment
                                , drawTextText :: T.Text
                                }
                                deriving (Show, Eq)

data PreparedText = PreparedText    { preparedText :: PreparedWrapText
                                    , preparedTextDef :: DrawTextDef
                                    , preparedTextFont :: TTFFont
                                    }
                                    deriving (Show)

prepareText :: MonadIO m => Widget -> Skin -> DrawTextDef -> m PreparedText
prepareText widget skin d@DrawTextDef{..} = do
    fnt <- runProxyCanvas widget $ getFont drawTextFontKey
    let (SDL.Rectangle p' (V2 w' h')) = drawTextRect
    case drawTextWrap of
      TextNoWrap{..} -> do
        txSz@(V2 txW txH) <- P.strSize fnt $ T.unpack drawTextText
        let (SDL.Rectangle p0@(P (V2 x _)) _) = rectAlign drawTextAlignment txSz drawTextRect
            extendedW = if txW > w' then min txW $ max textAreaMaxWidth w' else w'
            r = SDL.Rectangle p' (V2 extendedW (max txH h'))
{-        liftIO $ putStrLn $ concat ["prepareText  \"", T.unpack drawTextText, "\"  txSz=", show txSz,
            " drawTextRect=", rectToBriefStr drawTextRect,
            "  extendedW=", show extendedW,
            "  r0=", rectToBriefStr r0,"  r=", rectToBriefStr r] -}
        return $ PreparedText (PreparedWrapText (V.singleton drawTextText) (VU.singleton p0) x txH)
                    d{drawTextRect=r} fnt
      TextWrap {..} -> do
        t@(PreparedWrapText _ _ _ prepH) <- prepareWrapText drawTextAlignment drawTextRect
            (fromMaybe (formTextLineSpacing skin) textAreaLineSpacing) fnt drawTextText
        let h2 = if (prepH > h') && (textAreaMaxHeight > 0) then min prepH textAreaMaxHeight else h'
        return $ PreparedText t d{drawTextRect=SDL.Rectangle p' (V2 w' h2)} fnt

drawPreparedText :: MonadIO m => PreparedText -> GuiColor -> Maybe GuiColor -> Canvas m ()
drawPreparedText PreparedText{..} color mbBkColor =
    withClipRect (drawTextRect preparedTextDef) $ drawSplitPreparedText preparedText preparedTextFont color mbBkColor
{-# INLINE drawPreparedText #-}

textWrapModeToMbBkColor :: TextWrapMode -> Skin -> GuiColor -> Maybe GuiColor
textWrapModeToMbBkColor TextNoWrap{} _ c = Just c
textWrapModeToMbBkColor TextWrap{textAreaLineSpacing=s} skin c
    | fromMaybe (formTextLineSpacing skin) s < 1 = Nothing
    | otherwise = Just c
{-# INLINE textWrapModeToMbBkColor #-}