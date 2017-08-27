{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.Window.MessageBox
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Диалоговое окно выбора файла(ов) для загрузки или сохранения.

module GUI.Window.LoadSaveDialog(
    -- * Типы используемые в loadSaveDialog
    LoadOrSave(..),DirectoryItemColumn(..),LoadSaveDialogState(..),LoadSaveDialogDef(..)
    -- * Функция создаёт модальное окно для выбора файла(ов), для загрузки или сохранения.
    ,loadSaveDialog
    ) where


import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.IORef
import Control.Exception.Safe
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM
import Control.Monad.Extra (whenM,unlessM)
import Data.Default
import qualified TextShow as TS
import           TextShow (showb)
import TextShow.Control.Exception ()
import qualified SDL
import SDL.Vect
import SDL.Font (Font)
import GUI
import qualified GUI.BaseLayer.Primitives as P
import Data.Container.DirectAccess
import GUI.Utils.ViewableItems
import GUI.Widget.Layout.LinearLayout
import GUI.Widget.EditBox
import GUI.Widget.ListView
import GUI.Widget.Header
import GUI.Window.MessageBox
import GUI.Widget.DropDownList
import GUI.Widget.Button
import System.FileSystem
import GUI.Widget.PathBox
import Control.Monad.Auxiliaries

pattern PaddingX :: Coord; pattern PaddingX = 5
pattern WindowH  :: Coord; pattern WindowH  = 600
pattern ButtonW  :: Coord; pattern ButtonW  = 100
pattern ButtonH  :: Coord; pattern ButtonH  = 20

-- | Тип диалога выбора файла.
data LoadOrSave = LoadDialog | LoadMultiSelectDialog | SaveDialog
        deriving (Eq)

-- | Запись в которой хранится состояние настройки диалога. Может быть использована для
-- сохранения и воостановления настроек между вызовами @loadSaveDialog@.
-- Передаётся в 'LoadSaveDialogDef', возвращается как аргумент функции успешного выбора файл(ов)
-- передаваемой @loadSaveDialog@ последним аргументом.
data LoadSaveDialogState = LoadSaveDialogState  {
          loadSaveStExtsVarIx :: Int -- ^ Начальный номер ряда списка выбора срасширений.
        , loadSaveStSortCol     :: DirectoryItemColumn
        , loadSaveStSortMode    :: SortMode
        , loadSaveStNameWidth :: Coord
        , loadSaveStExtWidth :: Coord
        , loadSaveStSizeWidth :: Coord
        , loadSaveStTimeWidth :: Coord
                                                }

instance Default LoadSaveDialogState where
    def = LoadSaveDialogState {
              loadSaveStExtsVarIx = 0
            , loadSaveStSortCol = DiExt
            , loadSaveStSortMode = Ascending
            , loadSaveStNameWidth = 400
            , loadSaveStExtWidth = 50
            , loadSaveStSizeWidth = 150
            , loadSaveStTimeWidth = 100
                              }

data LoadSaveDialogDef = LoadSaveDialogDef  {
          loadSaveState :: LoadSaveDialogState
        , loadSaveTitle :: T.Text
          -- | @["Исходники Haskell|.hs;.lhs;.hs-boot;.lhs-boot"]@ или @[".hs;.lhs;.hs-boot;.lhs-boot"]@
        , loadSaveExts :: [T.Text]
--        , loadSaveMultiSelect :: Bool
                                            }

instance Default LoadSaveDialogDef where
    def = LoadSaveDialogDef {
          loadSaveState = def
        , loadSaveTitle = T.empty -- ^ Заголовок окна по умолчанию.
        , loadSaveExts = [".*"] -- ^ Show as *.*
--        , loadSaveMultiSelect = False
                            }

-- | Создаёт модальное окно для выбора файла(ов), для загрузки или сохранения.
loadSaveDialog :: MonadIO m => Gui -> -- ^ Ссылка на GUIRecord
                           LoadOrSave -> -- ^ Тип диалога.
                           -- | Директория и, возможно, прелагаемое имя файла.
                           -- Если директории нет - предполагается текущая директория.
                           -- Если строка пуста, предполагается текущая директория и любой файл.
                           FilePath ->
                           LoadSaveDialogDef -> -- ^ Дополнительные параметры диалога и его состояние.
                           -- | Функция - - обработчик завершения, получает состояние диалога и
                           -- список выбранных файлов.
                           (forall n. MonadIO n => LoadSaveDialogState -> V.Vector FilePath -> n ()) ->
                           m ()
loadSaveDialog gui loadOrSave initFilePath LoadSaveDialogDef{..} answerFn = do
    let sayErr :: MonadIO m => TS.Builder -> m ()
        sayErr b = say gui MsgBoxError $ "loadSaveDialog : " <> b
--        sayOnErr :: MonadIO m => IO a -> (forall n. MonadIO n => a -> n b) -> m (Maybe b)
        sayOnErr f1 f2 = do either0 <- liftIO $ tryAny f1
                            case either0 of
                                Left e0 -> sayErr (showb (e0 :: SomeException)) >> return Nothing
                                Right a -> Just <$> f2 a

    skin <- guiGetSkin gui
    rfS <- newMonadIORef loadSaveState
    rf <- liftIO (VM.new 0 >>= newIORef)
    title <- if T.null loadSaveTitle then
                  getT gui $ case loadOrSave of
                                LoadDialog -> "openFile"
                                LoadMultiSelectDialog -> "openFiles"
                                SaveDialog -> "saveFile"
             else return loadSaveTitle
    colTitles <- mapM (getT gui) ["colName","colExt","colSize","colTime"]
    let colWidthLst = [loadSaveStNameWidth loadSaveState, loadSaveStExtWidth  loadSaveState,
                       loadSaveStSizeWidth loadSaveState, loadSaveStTimeWidth loadSaveState]
        tableW = sum colWidthLst
        (MarginLTRB lMarg _ rMarg yMarg) = marginToLTRB $ formItemsMargin skin
        (extsDescr,exts) =
            let extsSplit = T.split (==';')
                go :: T.Text -> (T.Text,[FilePath])
                go s = let (d,e) = T.span (/='|') s in
                       map T.unpack <$> if T.null e then let es = extsSplit d in
                                                         (T.intercalate "; " $ map (T.cons '*') es,es)
                                        else (d, extsSplit $ T.tail e)
            in unzip $ map go loadSaveExts

    win <- newModalWindow' gui title WindowCloseOnEsc SDL.defaultWindow{
            SDL.windowInitialSize =
                P.toSDLV2 $ V2 (lMarg + tableW + rMarg)
                               WindowH
            }

    vL <- win $+ vLayout def
    pthBx <- vL $+ pathBox def{
          pathBoxFormItemDef = def{formItemMargin = Just $ WidgetMarginXY lMarg 5}
        , pathBoxWidth = tableW
        , pathBoxCanMkDir = loadOrSave == SaveDialog
                         }

    let getPth :: MonadIO m => m FilePath
        getPth = getValue pthBx
        setPth  :: MonadIO m => FilePath -> m ()
        setPth = setValue pthBx

    hdr <- vL $+ header def{
          headerColumns = V.fromList $ zip colTitles colWidthLst
        , headerSortMode = Just (fromEnum $ loadSaveStSortCol loadSaveState, loadSaveStSortMode loadSaveState)
                           }
    lv <- vL $+ listView def{
          listViewFormItemDef = def{formItemMargin = Just $ WidgetMarginLTRB lMarg 0 rMarg yMarg}
        , listViewSize = V2 tableW (-1)
        , listViewListViewFlags = if loadOrSave == LoadMultiSelectDialog then
                                        listViewListViewFlags def .|. MultiSelectListViewFlag
                                  else  listViewListViewFlags def
                            } $ ListViewDirectory rfS rf
    ebFile <- vL $+ editBox def{
          editBoxWidth = tableW
        , editBoxMaxChar = 1000 -- ?
        , editBoxCharFilter = isLegalPathSymbol
                         }
    hLExt <- vL $+ hLayout def
    ddlExt <- hLExt $+ dropDownList def{ ddListSize = V2 (tableW-6*lMarg-2*ButtonW)   ButtonH
                                       , ddListIx = loadSaveStExtsVarIx loadSaveState
                                       } $ ListViewText (V.fromList extsDescr)
    okT <- getT gui $ if loadOrSave == SaveDialog then "letSave" else "letOpen"
    btOk <- hLExt $+ button def{ btnSize = V2 ButtonW ButtonH
                                    , btnText = okT
                                    }
    cancelT <- getT gui "cancelB"
    btCancel <- hLExt $+ button def{  btnSize = V2 ButtonW ButtonH
                                    , btnText = cancelT
                                    }

    rfTerminated <- newMonadIORef False

    let reSort :: MonadIO m => m ()
        reSort = do
            v <- readMonadIORef rf
            LoadSaveDialogState{..} <- readMonadIORef rfS
            liftIO $ sortDirectoryItems loadSaveStSortCol loadSaveStSortMode v
            markWidgetForRedraw $ baseWidget lv

        update :: MonadIO m => FilePath -> m Bool
        update pth = do
            absPth <- let pth' = dropTrailingPathSeparator pth in
                      if isAbsolute pth' then return $ Just pth'
                      else do
                        p <- getPth
                        p' <- if null p then sayOnErr getCurrentDirectory return
                              else return $ Just p
                        onJustM p' $ \ p0 -> sayOnErr (makeAbsolute $ p0 </> pth') return
            case absPth of
              Just p -> do
                isDir <- liftIO $ doesDirectoryExist p
                let (path,file) = if isDir then (p,"") else splitFileName p
                setPth path
                setText ebFile $ T.pack file
                loadDir
                return True
              _ -> return False

        exit :: MonadIO m => V.Vector FilePath -> m ()
        exit v =    let doExit :: MonadIO m => m ()
                        doExit = unlessM (readMonadIORef rfTerminated) $ do
--                                    liftIO (putStr "doExit : v = " >> print v)
                                    state <- readMonadIORef rfS
                                    writeMonadIORef rfTerminated True
                                    delWindow win
                                    answerFn state v
                    in if not (V.null v) && (loadOrSave == SaveDialog) then do
                            let fpth = V.head v
                            exist <- liftIO $ doesFileExist fpth
                            if exist then do
                                msg <- (TS.fromText . T.replace "%"
                                            (T.pack $ takeFileName fpth ))
                                            <$> getT gui "fileOverWarn"
                                messageBox gui MsgBoxWarningYesNo msg {- $ \bt -> do
                                        liftIO (putStr "doExit, over confirm : btCode = " >>
                                               putStr (show $ fromEnum bt) >>
                                               putStr " v = " >> print v  ) -}
                                        ((`when` doExit).(== ButtonYes)) -- bt
                            else doExit
                       else doExit

        onSuccess :: MonadIO m => m ()
        onSuccess = do
            p <- getPth
            file <- T.unpack <$> getText ebFile
            if null file then do
                v <- readMonadIORef rf
                when (VM.length v > 0) $ do
                    markers <- getMarkers lv
                    let markersLn = VU.length markers
                        ixs = VU.map snd $ VU.filter fst $ VU.zip markers $ VU.generate markersLn id
                        cnt = VU.length ixs

                        mkFullPath :: Int -> IO (FilePath,Bool)
                        mkFullPath ix = do
                            DirectoryItem{..} <- VM.read v ix
                            return (p </> T.unpack diName <.> T.unpack diExt, diIsDir)
                    r <- if | cnt > 0 -> liftIO $ V.generateM cnt
                                (fmap fst . mkFullPath . (ixs VU.!))
                            | otherwise -> do
                                (s,isDir) <- getIx lv >>= liftIO . mkFullPath
                                return $ if isDir then V.empty
                                     else V.singleton s
                    exit r
            else exit $ V.singleton $ p </> file

        loadDir :: MonadIO m => m ()
        loadDir = do
            p <- getPth
            LoadSaveDialogState{..} <- readMonadIORef rfS
            ep <- liftIO $ tryAny $ loadDirectoryItems (exts !! loadSaveStExtsVarIx) p
            writeMonadIORef rf =<<
                (case ep of
                    Left e -> sayErr (showb e) >> liftIO (VM.new 0)
                    Right ctx -> return ctx)
            reSort

    onChanged ddlExt $ \extIx -> do
        modifyMonadIORef' rfS $ \d ->  d{loadSaveStExtsVarIx=extIx}
        loadDir

    onWidthsChange hdr $ \widths -> do
        modifyMonadIORef' rfS $ \d ->  d{ loadSaveStNameWidth = widths V.! fromEnum DiName
                                        , loadSaveStExtWidth  = widths V.! fromEnum DiExt
                                        , loadSaveStSizeWidth = widths V.! fromEnum DiSize
                                        , loadSaveStTimeWidth = widths V.! fromEnum DiTime
                                        }
        reSort

    onSortChange hdr $ \col sm ->  do
        modifyMonadIORef' rfS $ \d -> d{ loadSaveStSortCol = toEnum col, loadSaveStSortMode  = sm}
        reSort

    onClick btOk onSuccess

    onDoubleClick1 lv $ \i -> do
        v <- readMonadIORef rf
        when ((i>=0) && (i< VM.length v)) $ do
            DirectoryItem{..} <- liftIO $ VM.read v i
            let s = T.unpack diName
            if diIsDir then
                getPth >>= setPth . (</> s) >> loadDir
            else setText ebFile (T.pack $ s <.> T.unpack diExt) >> onSuccess

    onEnd ebFile $ \ t -> unless (T.null t) $
        whenM (update $ T.unpack t) $ unlessM(T.null <$> getText ebFile)
            onSuccess

    onClick btCancel $ exit V.empty
    setWinOnClosed win $ \_ -> exit V.empty

    setFocus ebFile
    void $ update initFilePath

-- | Подготовленные данные для отрисовки элементов каталога с помощью @listView@.
data DirectoryPrepare = DirectoryPrepare
        Font -- ^ Шрифт
        SDL.Texture -- ^ Текстура с изображение директории.
        GuiSize -- ^ Размер текстуры.
        String -- ^ Формат даты и времени.

-- | Обёртка для передачи всех данных для отрисовки элементов каталога к @listView@.
data ListViewDirectory = ListViewDirectory (IORef LoadSaveDialogState) (IORef DirectoryItems)

instance ViewableItems ListViewDirectory DirectoryPrepare where
    viewablePrepare gui _skin _container = do
        (itemH,fnt) <- listViewPrepareTextOnly
        dirTexture <- getTexture "folder.png"
        tSz@(V2 _ tH) <- getTextureSize dirTexture
        timeFmt <- T.unpack <$> getT gui "dateTimeFmt"
        return (max tH itemH, DirectoryPrepare fnt dirTexture tSz timeFmt)

    viewableDrawItem _gui skin (SDL.Rectangle (P (V2 x0 yt)) (V2 w h))
            decoreState _isEna _isFocused _isMarked _isCur (DirectoryPrepare fnt dirTexture (V2 tW tH) timeFmt)
            (ListViewDirectory rfS rf) ix = do
        LoadSaveDialogState{..} <- readMonadIORef rfS
        DirectoryItem{..} <- getItemDARO rf ix -- liftIO (readIORef rf >>= (`VM.read` ix))
        when diIsDir $
            drawTexture dirTexture $ P $ V2 (x0+PaddingX) $ yt + ((h-tH) `div` 2)
        let drawt align xl xr = drawTextAligned fnt align (decoreFgColor decoreState)
                                    (DrawStrOpaque (decoreBkColor decoreState))
                                    (SDL.Rectangle (P (V2 (xl+PaddingX) yt)) (V2 (xr-xl-2*PaddingX) h))
--            drawv x = drawLine (P (V2 x yt)) (P (V2 x (yt+h)))
            xExt = x0+loadSaveStNameWidth
            xSize = xExt + loadSaveStExtWidth
            xTime = xSize + loadSaveStSizeWidth
        setColor $ cellBorderColor skin
        drawt AlignLeftCenter (x0+PaddingX+tW) xExt diName
--        drawv xExt
        drawt AlignLeftCenter xExt xSize diExt
--        drawv xSize
        unless diIsDir $
            drawt AlignRightCenter xSize xTime $ size2T diSize
--        drawv xTime
        drawt AlignLeftCenter xTime (x0+w) $ time2T timeFmt diTime

    viewableCanSelect _gui (ListViewDirectory _ rf) ix = (not . diIsDir) <$> getItemDARO rf ix

    viewableGetCount (ListViewDirectory _ rf) = sizeDARO rf
