{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.Resource
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <KldznDvr@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Менеджер ресурсов GUI приложения.
--
-- Менеджер ресурсов может загружать графические файлы, шрифты, курсоры и строки на выбранном естественном языке.
-- Всё это, в данном контексте, будем называть русурсами.
-- Загруженные ресурсы кешируются. При попытке загрузить точно такой же ресурс
-- (для шрифтов и с теми же параметрами) будет возвращаться сохранённый в памяти ресурс.
-- Ресурсы загруженные с помощью менеджера ресурсов не следует удалять вручную, не через менеджер ресурсов.
-- Менеджер ресурсов удаляет загруженные ресурсы при завершении приложения.
--
-- Содержимое графических файлов менеджер ресурсов хранит в данных типа 'SDL.Surface', т.е.
-- не зависящих от конкретного окна GUI. У каждого окна GUI есть свой кеш изображений в данных типа
-- 'SDL.Texture', в которые преобразуются 'SDL.Surface' получаемые от менеджера ресурсов.
-- Таким образом кеширование изображений получается двухуровневое. Когда виджет одного окна запросил
-- загрузку изображения (например, с помощью функции @GUI.BaseLayer.Canvas.getTexture@),
-- менеджер ресурсов загружает изображение из файла в  виде 'SDL.Surface' и сохраняет в своём кеше.
-- Далее 'SDL.Surface' преобразуется в 'SDL.Texture' и сохраняется в кеше данного окна.
-- Если виджет другого окна запросит тот же файл, его не будет в виде 'SDL.Surface', в кеше его окна,
-- но оно будет взято из кеша менеджера ресурсов в виде 'SDL.Surface' и снова преобразовано в 'SDL.Texture'
-- для другого окна. Это необходимо, т.к. конечный тип, котрый можно вывести на экран - 'SDL.Texture',
-- привязано в SDL к конкретному окну (точнее к его рендереру).
-- Строка передаваемая функциям загрузки графических ресурсов (в менеджере ресурсов это @rmGetSurface@,
-- но в виджетах следует использовать функции из "GUI.BaseLayer.Canvas") одновременно является как именем
-- графического файла (с расширением, и, возможно, путём к нему), так и текстовым ключём по которому
-- кешируется ресурс.
-- Если графический файл не найден, или его не удаётся превратить в 'SDL.Surface', менеджер ресурсов
-- создаст вместо него красный квадрат размером 5x5 пикселей, что и будет сигнализировать на экране о
-- проблеме с загрузкой файла.
--
-- Шрифты кешируются вместе с их настройками. То есть, если нужно использовать щрифт из одного и того же файла
-- но один без атрибута bold, а другой с ним, и/или разного размера - они считаются за разные ресурсы которые
-- следует загружать отдельно.
-- Для шрифтов имя файла и ключ поиска шрифта в кеше различаются и задаются отдельно.
-- Некоторые виджеты предполагают наличие шрифта с заданным ключом. Например, виджеты
--  @GUI.Widget.Label.label@ и @GUI.Widget.Button.button@ по умолчанию используют ключ \"label\",
-- а виджет @GUI.Widget.EditBox.editBox@ ключ \"edit\" для запроса шрифта.
-- Если шрифт с указанным ключом не найден, делается попытка загрузить шрифт с ключом из пустой строки,
-- если и его нет, то первый же в кеше, и только если кеш пуст, будет выдано сообщение обошибке.
--
-- Кеширование курсоров по умолчанию ограничено загрузкой при старте приложения системных курсоров,
-- но, при желании можно неким путём получить курсор и добавить его как пользовательский.
--
-- Поиск ресурсов :
--
-- Если путь к ресурсу (например, графическому файлу) задан не абсолютный или не задан вообще
-- (а так обычно и предполагается), он ищется в директории ресурсов. Только для графических файлов
-- (не шрифтов), поиск вначале выполняется в поддректории директории ресурсов с именем /ИмяСкина.skin/,
-- где /ИмяСкина/ - имя из поля @GUI.BaseLayer.Depend1.Skin.skinName@ текущего скина, и если файл не найден там, то в
-- самой директории ресурсов.
--
-- Директория ресурсов выбирается по следующему алгоритму:
--    - Если есть переменная окружения ИМЯПРИЛОЖЕНИЯ_GUIRESOURCES, то берётся оттуда;
--
--    - Наличие поддиректории GUI.Resources по пути явно указанном в @GUI.BaseLayer.RunGUI.guiDataDirectory@.
--
--    - Проверяется наличие поддиректории GUI.Resources по пути возвращаемом
--      @getXdgDirectory XdgData \"ИмяПриложения\"@ (см. документацию к пакету __directory__);
--
--    - Проверяется наличие поддиректории GUI.Resources по пути исполняемого файла.
--      Если в этом пути находится директория __/.stack-work/__ , то оставляется путь левее этой директории.
--
-- Директория ресурсов должна существовать к моменту выполнения функции @GUI.BaseLayer.RunGUI.runGUI@.

module GUI.BaseLayer.Resource(
    -- * Основные типы и функции менеджера ресурсов.
    ResourceManager,initResourceManager,destroyResourceManager
    -- * Функции относящиеся к графическим файлам.
    ,rmGetSurfaceFromCache,rmGetSurface,rmAddSurface
    -- * Типы и функции относящиеся к графическим файлам.
    ,GuiFontOptions(..),GuiFontDef(..),rmGetFont,rmLoadFont,fontTuning
    -- * Функции относящиеся к курсорам.
    ,rmGetCursor,rmSetCursor,rmAddCursor
    -- * Функции подстановки натурального языка
    ,rmGetB
    ) where

import Data.List
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Safe
import System.FilePath
import System.Directory
import System.Environment
import System.Exit (exitFailure)
import Data.Char
import Data.IORef
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import Control.Monad.Extra (whenJust,unlessM)
import qualified TextShow as TS
import qualified SDL
import SDL.Vect
import qualified SDL.Font as FNT
import SDL.Font (Font)
import qualified SDL.Image as IMAGE
import GUI.BaseLayer.Depend0.Types
import GUI.BaseLayer.Depend0.Ref
import GUI.BaseLayer.Depend0.Cursor
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend1.Logging
import GUI.BaseLayer.Depend1.Resource
import GUI.BaseLayer.NaturalLangIO

pattern DefResourceSubDirectory :: String
pattern DefResourceSubDirectory         = "GUI.Resources"
pattern SkinDirectorySuffix :: String
pattern SkinDirectorySuffix             = ".skin"
pattern EnvResourceDirectoryPathSuffix :: String
pattern EnvResourceDirectoryPathSuffix  = "_GUIRESOURCES"
pattern StackWorkDirectory :: String
pattern StackWorkDirectory  = ".stack-work"
pattern ErrSurfaceDimension :: Coord
pattern ErrSurfaceDimension             = 5

-- | Функция инициализации менеджера ресурсов. Вызывается только один раз из @GUI.BaseLayer.RunGUI.runGUI@
initResourceManager :: MonadIO m =>
                        String -> -- ^ Имя скина.
                        [GuiFontDef] -> -- ^ Список описаний шрифтов для начальной загрузки.
                        String -> -- ^ Директория ресурсов (определяется в @GUI.BaseLayer.RunGUI.runGUI@).
                        String -> -- ^ Название языка, например "en-En" or "ru-Ru" - имя поддиректории.
                        GUILog -> -- ^ Журнал приложения для вывода сообщений обошибках.
                        m ResourceManager
initResourceManager skinName fntLst dataDirectory uiLang gLog = do
--  liftIO (putStr "Displays : " >> SDL.getDisplays >>= print)
    appName <- liftIO getAppName
    -- for ex. set GUIDEMO_GUIRESOURCES=c:\...\GUI.Resources
    let envParamName = map toUpper appName ++ EnvResourceDirectoryPathSuffix
        dirMsg dir = "Directory " <> TS.fromString dir <> " is not found.\n"
        guiTerminated msg = do
            logPutLn gLog msg
            liftIO $ showErrMsgBoxB msg
            logPutLn gLog "\nGUI terminated."
            liftIO exitFailure
    mbP <- liftIO $ lookupEnv envParamName
--    liftIO $ putStrLn $ "mbP=" ++ show mbP
    p <- case mbP of
        Just p1 -> do
            exist1 <- liftIO $ doesDirectoryExist p1
            if exist1 then return p1
            else guiTerminated $ "Environment variable " <> TS.fromString envParamName <>
                    " pointed to the resources directory.\n" <> dirMsg p1
        _ -> do
            -- for ex. C:\Users\User\AppData\Roaming\GUIDemo\GUI.Resources or ~/.local/share/GUIDemo/GUI.Resources
            let p0 = dataDirectory </> DefResourceSubDirectory
--            liftIO $ putStrLn $ "p0=" ++ p0
            exist0 <- liftIO $ doesDirectoryExist p0
            if exist0 then return p0
            else do
--                p1 <- ((</> DefResourceSubDirectory).takeDirectory) <$> liftIO getExecutablePath
                exeDir <- takeDirectory <$> liftIO getExecutablePath
                let spltd = splitPath exeDir
                    p' = case elemIndex (addTrailingPathSeparator StackWorkDirectory) spltd of
                            Just i -> joinPath $ take i spltd
                            _ -> exeDir
                    p1 = p' </> DefResourceSubDirectory
                exist1 <- liftIO $ doesDirectoryExist p1
                if exist1 then return p1
                else guiTerminated $ "Search for resources directory is unsuccessful :\n" <>
                        dirMsg p0 <> dirMsg p1 <>
                        "Environment variable " <> TS.fromString  envParamName <> " is not found."

--    liftIO $ putStrLn $ "p=" ++ p
    let resP = addTrailingPathSeparator p
        langP = resP </> "I18n" </> uiLang
--    liftIO $ putStrLn $ "initResourceManager langP=" ++ langP
    natS <- liftIO $ loadLangFiles gLog langP
--    liftIO $ print natS
    sf <- newMonadIORef HM.empty
    fnts <- newMonadIORef HM.empty
    sysCursors <- mkSystemCursorSet
    userCursorsHM <- newMonadIORef HM.empty
    let rm = ResourceManager { resPath  = resP
                            , skinPath = addTrailingPathSeparator $ resP </> (skinName ++ SkinDirectorySuffix)
                            , systemCursorSet = sysCursors
                            , userCursors = userCursorsHM
                            , surfaces = sf
                            , fonts = fnts
                            , natStrs = natS
                            }
    mapM_ (rmLoadFont rm) fntLst
    return rm;

-- | Функция освобождения ресурсов захваченных менеджером ресурсов.
-- Вызывается только один раз из @GUI.BaseLayer.runGUI.runGUI@ при завершении работы приложения
destroyResourceManager:: MonadIO m => ResourceManager -> m ()
destroyResourceManager r = do
--    mapM_ SDL.destroySurface =<< readMonadIORef (surfaces r)
    mapM_ SDL.freeSurface =<< readMonadIORef (surfaces r)
    mapM_ freeCursor =<< readMonadIORef (userCursors r)
    mapM_ (FNT.free . fnt) =<< readMonadIORef (fonts r)
    freeCursorSet (systemCursorSet r)

-- no exported
rmGetPath:: MonadIO m => ResourceManager -> String -> m FilePath
rmGetPath _ s | isAbsolute s = return s
rmGetPath r s = do
    let p = skinPath r ++ s
    chk <- liftIO $ doesFileExist p
    if chk
    then return p
    else return $ resPath r ++ s

-- no exported
class ResourceManagerCacheable a where
    getResourceManagerCollectionField:: ResourceManager -> IORef (CacheCollection a)

-- no exported
toCache :: (MonadIO m, ResourceManagerCacheable a) => ResourceManager -> T.Text -> a -> m ()

-- no exported
toCache r k v  = modifyMonadIORef' (getResourceManagerCollectionField r) (HM.insert k v)

-- no exported
fromCache :: (MonadIO m, ResourceManagerCacheable a) => ResourceManager -> T.Text -> m (Maybe a)

-- no exported
fromCache r k = HM.lookup k <$> readMonadIORef (getResourceManagerCollectionField r)

instance ResourceManagerCacheable SDL.Surface where
    getResourceManagerCollectionField  = surfaces

instance ResourceManagerCacheable FontCollectionItem where
    getResourceManagerCollectionField  = fonts

instance ResourceManagerCacheable GuiCursor where
    getResourceManagerCollectionField  = userCursors

-- no exported
rmLoadValue:: MonadIO m => ResourceManager -> (FilePath -> IO a) -> String -> m a -> m a
rmLoadValue r f k def = do
            path <- rmGetPath r k
            mv <- liftIO $ try $ f path
            case mv of
                    Left (_ :: SomeException) -> {- (liftIO $ putStrLn "rmLoadValue, SomeException") >> -} def
                    Right x -> return x

-- no exported
rmGetValue:: (MonadIO m, ResourceManagerCacheable a) => ResourceManager -> (FilePath -> IO a) -> T.Text -> m a -> m a
rmGetValue r f k def = do
    m <- fromCache r k
    case m of
        Just v -> return v
        _ -> do
            v <- rmLoadValue r f (T.unpack k) def
            toCache r k v
            return v

-- | Запросить 'SDL.Surface' из кеша, но не загружать из файла.
rmGetSurfaceFromCache:: MonadIO m => ResourceManager -> T.Text -> m (Maybe SDL.Surface)
rmGetSurfaceFromCache = fromCache
{-# INLINE rmGetSurfaceFromCache #-}

-- | Запросить 'SDL.Surface'. Вначале ищется в кеше, если в кеше нет, загружается из файла.
-- Поддерживаемые форматы и расширения : .BMP, .PNG, .JPG, .JPEG , .GIF, .ICO
rmGetSurface:: MonadIO m => ResourceManager -> T.Text -> m SDL.Surface
rmGetSurface r k = rmGetValue r load k def
    where load :: FilePath -> IO SDL.Surface
          load path = let ext = map toUpper $ takeExtension (T.unpack k) in
--                      liftIO $ putStrLn $ concat ["rmGetSurface.load path = ",path]
                      if | ext == ".BMP" ->
                            SDL.loadBMP path
-- from https://www.stackage.org/haddock/lts-8.13/sdl2-image-2.0.0/SDL-Image.html
-- PNG, JPG, TIF, GIF, WEBP, CUR, ICO, BMP, PNM, XPM, XCF, PCX and XV formatted data
                         | ext `elem` [".PNG",".JPG",".JPEG",".GIF",".ICO"] -> IMAGE.load path
                         | otherwise -> def
          def :: MonadIO m => m SDL.Surface
          def = do -- liftIO $ putStrLn "def"
                    let sz = V2 ErrSurfaceDimension ErrSurfaceDimension
                    surf <- ((SDL.displayModeFormat . head . SDL.displayModes . head) <$> SDL.getDisplays)
                        >>= SDL.createRGBSurface (fmap fromIntegral sz)
                    SDL.surfaceFillRect surf Nothing (V4 255 0 0 0)
                    return surf

-- | Добавить в кеш 'SDL.Surface' полученный не через менеджер ресурсов.
-- После добавления в кеш не освобождать 'SDL.Surface' вручную.
rmAddSurface:: MonadIO m => ResourceManager -> T.Text -> SDL.Surface -> m ()
rmAddSurface r abbr surface = do
    sfc <- readMonadIORef $ surfaces r
    (case HM.lookup abbr sfc of
            Just osf -> SDL.freeSurface osf >> return (HM.adjust (const surface) abbr sfc)
            _ -> return $ HM.insert abbr surface sfc)
        >>= writeMonadIORef (surfaces r)

-- | Запросить шрифт из кеша по ключу.
rmGetFont:: MonadIO m => ResourceManager -> T.Text -> m Font
rmGetFont r abbr = do
    fc <- readMonadIORef $ fonts r
    case HM.lookup abbr fc of
        Just f -> return (fnt f)
        _ -> case HM.lookup T.empty fc of
            Just fDef -> return $ fnt fDef
            _ | HM.null fc -> error "No fonts loaded"
            _ -> return $ fnt $ fc HM.! head (HM.keys fc)

-- | Загрузить шрифт, настроить как задано и сохранить в кеше.
-- Предпочтительно загружать все шрифты при запуске приложения указывая их в списке @initResourceManager@.
rmLoadFont:: MonadIO m => ResourceManager -> GuiFontDef -> m Font
rmLoadFont r (GuiFontDef abbr pth fntSz opts) = do
    fc <- readMonadIORef $ fonts r
    e  <- case HM.lookup abbr fc of
                Just oldFnt | fntPath oldFnt == pth && fntPtSz oldFnt == fntSz && fntOpts oldFnt == opts
                                    -> return $ Left (fnt oldFnt)
                Just oldFnt -> FNT.free (fnt oldFnt) >> return (Right (HM.delete abbr fc))
                _ -> return $ Right fc
    case e of
        Left f -> return f
        Right fc2 -> do
            fci <- rmLoadValue r load pth
                     $ error $ concat ["Can\'t load font ",pth,",  Point size = ",show fntSz]
            fontTuning (fnt fci) opts
            writeMonadIORef (fonts r) $ HM.insert abbr fci fc2
            return $ fnt fci
 where load path = do
--                liftIO $ putStrLn $ "rmLoadFont.load path=" ++ path
                unlessM (doesFileExist path) $ error $ "Font file " ++ path ++ " does not exist"
                FontCollectionItem pth fntSz opts <$> FNT.load path fntSz

-- | Перенастройка параметров кешируемого шрифта.
-- Вы должны знать что вы делаете. Потребуется обновление виджетов использующих этот шрифт.
-- Во многих виджетах размеры и внутреннии координаты рассчитываются только при создании виджета
-- и могут зависеть от размеров шрифта. Т.е. эта операция может быть небезопасной в смысле
-- корректного отображения виджетов.
fontTuning :: MonadIO m => Font -> GuiFontOptions -> m ()
fontTuning fnt GuiFontOptions{..} = do
    whenJust fontKerning $ \ new -> do
        old <- FNT.getKerning fnt
        when (old /= new) $ FNT.setKerning fnt new
    whenJust fontHinting $ \ new -> do
        old <- FNT.getHinting fnt
        when (old /= new) $ FNT.setHinting fnt new
    whenJust fontOutline $ \ new -> do
        old <- FNT.getOutline fnt
        when (old /= new) $ FNT.setOutline fnt new
    whenJust fontStyle $ \ new -> do
        old <- FNT.getStyle fnt
        when (old /= new) $ FNT.setStyle fnt new
--    setFontStyleIfNeed fnt style

-- | Получить курсор из кеша по индексу курсора. См. "GUI.BaseLayer.Depend0.Cursor".
rmGetCursor:: MonadIO m => ResourceManager -> CursorIx -> m GuiCursor
rmGetCursor r (CursorResourceIx ix) = do
    uc <- readMonadIORef $ userCursors r
    case HM.lookup ix uc of
        Just c -> return c
        _ -> return $ getCursorByIx (systemCursorSet r) SystemCursorArrow
rmGetCursor r ix = return $ getCursorByIx (systemCursorSet r) ix

-- | Установить текущий курсор на экране из курсора в кеше по индексу.
-- Только для базового слоя GUI-SDL2. Не использовать в пользовательском коде.
rmSetCursor:: MonadIO m => ResourceManager -> CursorIx -> m ()
rmSetCursor r ix = setCursor =<< rmGetCursor r ix
{-# INLINE rmSetCursor #-}

-- | Добавить курсор созданный вне менеждера ресурсов.
-- После добавления не освобождать курсор вручную.
rmAddCursor:: MonadIO m => ResourceManager -> T.Text -> GuiCursor -> m ()
rmAddCursor r abbr cursor = do
    uc <- readMonadIORef $ userCursors r
    (case HM.lookup abbr uc of
            Just oc -> freeCursor oc >> return (HM.adjust (const cursor) abbr uc)
            _ -> return $ HM.insert abbr cursor uc)
        >>= writeMonadIORef (userCursors r)

rmGetB :: ResourceManager -> B.ByteString -> TS.Builder
rmGetB rm k = case HM.lookup k $ natStrs rm of
                Just t -> TS.fromText t
                _ -> "?" <> TS.showb (B.unpack k) <> "?"