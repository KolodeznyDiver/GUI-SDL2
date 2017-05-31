{-# LANGUAGE FlexibleInstances #-}  --  TypeFamilies TypeFamilyDependencies
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.BaseLayer.Resource(
    -- GUI.BaseLayer.Resource.Types
    GuiFontDef(..),ResourceManager
    -- GUI.BaseLayer.Resource
    ,initResourceManager,destroyResourceManager,rmGetTextureFromCache,rmGetTexture,rmAddTexture
    ,rmGetFont,rmLoadFont,rmGetCursor,rmSetCursor,rmAddCursor
    ) where

import qualified SDL
import SDL.Vect
--import qualified GUI.BaseLayer.Cursor as Raw
import qualified SDL.TTF as TTF
import SDL.TTF.FFI (TTFFont)
import qualified SDL.Image as IMAGE
--import Data.StateVar
import System.FilePath
import System.Directory
import System.Environment
import System.Exit (exitFailure)
import MonadUtils (unlessM) -- (fmapMaybeM)
import Data.Char
--import Maybes (whenIsJust)
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
--import Data.StateVar
import Data.IORef
-- import Control.Monad
import Control.Exception
import GUI.BaseLayer.Types
-- import GUI.BaseLayer.Internal.Types
import GUI.BaseLayer.Resource.Types
import GUI.BaseLayer.Ref
import GUI.BaseLayer.Cursor
--import GUI.BaseLayer.Geometry
import GUI.BaseLayer.Primitives
--import Foreign -- debug
--import qualified Data.Vector.Storable.Mutable as VM

pattern DefResourceSubDirectory :: String
pattern DefResourceSubDirectory         = "GUI.Resources"
pattern SkinDirectorySuffix :: String
pattern SkinDirectorySuffix             = ".skin"
pattern EnvResourceDirectoryPathSuffix :: String
pattern EnvResourceDirectoryPathSuffix  = "_GUIRESOURCES"
pattern ErrTextureDimension :: Coord
pattern ErrTextureDimension             = 5

initResourceManager :: MonadIO m => String -> [GuiFontDef] -> m ResourceManager
initResourceManager skinName fntLst = do
    appName <- takeBaseName <$> liftIO getProgName
    -- for ex. set GUIDEMO_GUIRESOURCES=c:\...\GUI.Resources
    let envParamName = map toUpper appName ++ EnvResourceDirectoryPathSuffix
        dirMsg dir = T.concat ["Directory ", T.pack dir, " is not found.\n" ]
        guiTerminated lst = do
            SDL.showSimpleMessageBox Nothing SDL.Error (T.pack appName) $ T.append
                (T.concat lst) "\n\nGUI terminated."
            liftIO exitFailure
    mbP <- liftIO $ lookupEnv envParamName
--    liftIO $ putStrLn $ "mbP=" ++ show mbP
    p <- case mbP of
        Just p1 -> do
            exist1 <- liftIO $ doesDirectoryExist p1
            if exist1 then return p1
            else guiTerminated [ "Environment variable ", T.pack  envParamName,
                    " pointed to the resources directory.\n", dirMsg p1]
        _ -> do
            -- for ex. C:\Users\User\AppData\Roaming\GUIDemo\GUI.Resources or ~/.local/share/GUIDemo/GUI.Resources
            p0 <- (</> DefResourceSubDirectory) <$> liftIO (getXdgDirectory XdgData appName)
--            liftIO $ putStrLn $ "p0=" ++ p0
            exist0 <- liftIO $ doesDirectoryExist p0
            if exist0 then return p0
            else do
                p1 <- ((</> DefResourceSubDirectory).takeDirectory) <$> liftIO getExecutablePath
                exist1 <- liftIO $ doesDirectoryExist p1
                if exist1 then return p1
                else guiTerminated [ "Search for resources directory is unsuccessful :\n",
                        dirMsg p0, dirMsg p1,
                        "Environment variable ", T.pack  envParamName, " is not found." ]

--    liftIO $ putStrLn $ "p=" ++ p
    let resP = addTrailingPathSeparator p
    tx <- newMonadIORef HM.empty
    fnts <- newMonadIORef HM.empty
    sysCursors <- mkSystemCursorSet
    userCursorsHM <- newMonadIORef HM.empty
    let rm = ResourceManager { resPath  = resP
                            , skinPath = addTrailingPathSeparator $ resP </> (skinName ++ SkinDirectorySuffix)
                            , systemCursorSet = sysCursors
                            , userCursors = userCursorsHM
                            , textures = tx
                            , fonts = fnts
                            }
    mapM_ (rmLoadFont rm) fntLst
    return rm;

destroyResourceManager:: MonadIO m => ResourceManager -> m ()
destroyResourceManager r = do
    mapM_ SDL.destroyTexture =<< readMonadIORef (textures r)
    mapM_ freeCursor =<< readMonadIORef (userCursors r)
    mapM_ (TTF.closeFont . fnt) =<< readMonadIORef (fonts r)
    freeCursorSet (systemCursorSet r)

rmGetPath:: MonadIO m => ResourceManager -> String -> m FilePath
rmGetPath _ s | isAbsolute s = return s
rmGetPath r s = do
    let p = skinPath r ++ s
    chk <- liftIO $ doesFileExist p
    if chk
    then return p
    else return $ resPath r ++ s {-do
        let p1 = (resPath r) ++ s
        chk <- doesFileExist p1
        return (if chk p1 else []) -}

class ResourceManagerCacheable a where
    getResourceManagerCollectionField:: ResourceManager -> IORef (CacheCollection a)

toCache :: (MonadIO m, ResourceManagerCacheable a) => ResourceManager -> T.Text -> a -> m ()
--toCache r k v  = newMonadIORef v >>= \rV -> modifyMonadIORef' (getResourceManagerCollectionField r) (HM.insert k rV)
toCache r k v  = modifyMonadIORef' (getResourceManagerCollectionField r) (HM.insert k v)

fromCache :: (MonadIO m, ResourceManagerCacheable a) => ResourceManager -> T.Text -> m (Maybe a)
--fromCache r k = (HM.lookup k <$> readMonadIORef (getResourceManagerCollectionField r)) >>= fmapMaybeM readMonadIORef
fromCache r k = HM.lookup k <$> readMonadIORef (getResourceManagerCollectionField r)

instance ResourceManagerCacheable SDL.Texture where
    getResourceManagerCollectionField  = textures

instance ResourceManagerCacheable FontCollectionItem where
    getResourceManagerCollectionField  = fonts

instance ResourceManagerCacheable GuiCursor where
    getResourceManagerCollectionField  = userCursors

rmLoadValue:: MonadIO m => ResourceManager -> (FilePath -> IO a) -> String -> m a -> m a
rmLoadValue r f k def = do
            path <- rmGetPath r k
            mv <- liftIO $ try $ f path
            case mv of
                    Left (_ :: SomeException) -> {- (liftIO $ putStrLn "rmLoadValue, SomeException") >> -} def
                    Right x -> return x

rmGetValue:: (MonadIO m, ResourceManagerCacheable a) => ResourceManager -> (FilePath -> IO a) -> T.Text -> m a -> m a
rmGetValue r f k def = do
    m <- fromCache r k
    case m of
        Just v -> return v
        _ -> do
            v <- rmLoadValue r f (T.unpack k) def
            toCache r k v
            return v
{-              do
          p <- rmGetPath r k
          m1 <- f p
          case m1 of
            Just v -> toCache r k v
            _ -> return ()
          return m1 -}

rmGetTextureFromCache:: MonadIO m => ResourceManager -> T.Text -> m (Maybe SDL.Texture)
rmGetTextureFromCache = fromCache
{-# INLINE rmGetTextureFromCache #-}

rmGetTexture:: MonadIO m => ResourceManager -> SDL.Renderer -> T.Text -> m SDL.Texture
rmGetTexture r renderer k = rmGetValue r load k def
    where load :: FilePath -> IO SDL.Texture
          load path = let ext = map toUpper $ takeExtension (T.unpack k) in
                      if | ext == ".BMP" -> do
--                            liftIO $ putStrLn "Before SDL.loadBMP"
                            bmp <- SDL.loadBMP path
--                            throwIfNoSurface bmp
                            SDL.createTextureFromSurface renderer bmp <* SDL.freeSurface bmp
-- from https://www.stackage.org/haddock/lts-8.13/sdl2-image-2.0.0/SDL-Image.html
-- PNG, JPG, TIF, GIF, WEBP, CUR, ICO, BMP, PNM, XPM, XCF, PCX and XV formatted data
                         | ext `elem` [".PNG",".JPG",".JPEG",".GIF",".ICO",".CUR"] -> IMAGE.loadTexture renderer path
                         | otherwise -> def
{-          throwIfNoSurface (SDL.Surface ptr mb) = liftIO $ putStrLn $ "Surface=" ++ show (ptrToIntPtr ptr) ++ "   " ++
                                                    (maybe "Nothing" (show . VM.length) mb) -}
          def :: MonadIO m => m SDL.Texture
          def = do -- liftIO $ putStrLn "def"
                    let sz = V2 ErrTextureDimension ErrTextureDimension
                    t  <- createTargetTexture renderer sz
                    withRendererTarget renderer t $ withRendererColor renderer (V4 255 0 0 0) $
                        SDL.fillRect renderer $ Just $ toSDLRect $ SDL.Rectangle zero sz
{- не отображается
                    surf <- getPixelFormat renderer >>= SDL.createRGBSurface (V2 ErrTextureDimension ErrTextureDimension)
                    SDL.surfaceFillRect surf Nothing (V4 255 0 0 0)
                    t <- SDL.createTextureFromSurface renderer surf <* SDL.freeSurface surf  -}

--                    ti <- SDL.queryTexture t
--                    liftIO $ putStrLn $ "def, TextureInfo =" ++ show ti
                    return t

rmAddTexture:: MonadIO m => ResourceManager -> T.Text -> SDL.Texture -> m ()
rmAddTexture r abbr texture = do
    tc <- readMonadIORef $ textures r
    (case HM.lookup abbr tc of
            Just t -> SDL.destroyTexture t >> return (HM.adjust (const texture) abbr tc)
            _ -> return $ HM.insert abbr texture tc)
        >>= writeMonadIORef (textures r)


rmGetFont:: MonadIO m => ResourceManager -> T.Text -> m TTFFont
rmGetFont r abbr = do
    fc <- readMonadIORef $ fonts r
    case HM.lookup abbr fc of
        Just f -> return (fnt f)
        _ -> case HM.lookup T.empty fc of
            Just fDef -> return $ fnt fDef
            _ | HM.null fc -> error "No fonts loaded"
            _ -> return $ fnt $ fc HM.! head (HM.keys fc)

rmLoadFont:: MonadIO m => ResourceManager -> GuiFontDef -> m TTFFont
rmLoadFont r (GuiFontDef abbr pth fntSz) = do
    fc <- readMonadIORef $ fonts r
    e  <- case HM.lookup abbr fc of
                Just oldFnt | fntPath oldFnt == pth && fntPtSz oldFnt == fntSz -> return $ Left (fnt oldFnt)
                Just oldFnt -> TTF.closeFont (fnt oldFnt) >> return (Right (HM.delete abbr fc))
                _ -> return $ Right fc
    case e of
        Left f -> return f
        Right fc2 -> do
            fci <- rmLoadValue r load pth
                     $ error $ concat ["Can\'t load font ",pth,",  Point size = ",show fntSz]
            writeMonadIORef (fonts r) $ HM.insert abbr fci fc2
            return $ fnt fci
 where load path = do
--                liftIO $ putStrLn $ "rmLoadFont.load path=" ++ path
                unlessM (doesFileExist path) $ error $ "Font file " ++ path ++ " does not exist"
                FontCollectionItem pth fntSz <$> TTF.openFont path fntSz

rmGetCursor:: MonadIO m => ResourceManager -> CursorIx -> m GuiCursor
rmGetCursor r (CursorResourceIx ix) = do
    uc <- readMonadIORef $ userCursors r
    case HM.lookup ix uc of
        Just c -> return c
        _ -> return $ cursorFromSystemIx (systemCursorSet r) SystemCursorArrow
rmGetCursor r ix = return $ cursorFromSystemIx (systemCursorSet r) ix

rmSetCursor:: MonadIO m => ResourceManager -> CursorIx -> m ()
rmSetCursor r ix = setCursor =<< rmGetCursor r ix
{-# INLINE rmSetCursor #-}

rmAddCursor:: MonadIO m => ResourceManager -> T.Text -> GuiCursor -> m ()
rmAddCursor r abbr cursor = do
    uc <- readMonadIORef $ userCursors r
    (case HM.lookup abbr uc of
            Just oc -> freeCursor oc >> return (HM.adjust (const cursor) abbr uc)
            _ -> return $ HM.insert abbr cursor uc)
        >>= writeMonadIORef (userCursors r)

