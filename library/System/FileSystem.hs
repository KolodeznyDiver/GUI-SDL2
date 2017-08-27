{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
-- |
-- Module:      System.FileSystem
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Типы и функции относящиеся к работе с файловой системой.

module System.FileSystem where

import Data.Maybe
import Data.Char
import System.Directory
import System.FilePath
import Data.Time
import qualified Data.Text as T
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Algorithms.Intro as A
import System.Utils (pattern IsWindows)
import GUI.Widget.Types (SortMode(..))

--import qualified Data.Text.IO as T -- for debug

data DirectoryItem = DirectoryItem  {
          diName :: T.Text
        , diExt :: T.Text
        , diSize :: Integer
        , diTime :: LocalTime
        , diIsDir :: Bool
                                    }

-- | Поля оглавления директории. Используется для указания режима сортировки.
data DirectoryItemColumn = DiName | DiExt | DiSize | DiTime
        deriving (Eq,Enum)

type DirectoryItems = VM.IOVector DirectoryItem


isLegalFileSymbol :: Char -> Bool
isLegalFileSymbol c = c == '.' || (IsWindows && (c==' ')) || isValid [c]

isLegalPathSymbol :: Char -> Bool
isLegalPathSymbol c = c == '.' || isPathSeparator c ||
                        (IsWindows && (c==' ' || c == ':')) || isValid [c]


dirCmpr :: (Ord a, Ord b) =>
           (DirectoryItem -> a) -> -- ^ Поле записи по которому сортировать файлы.
           (DirectoryItem -> b) -> -- ^ Поле записи по которому сортировать директории.
           SortMode -> -- ^ Направление сортировки.
           DirectoryItem -> DirectoryItem -> Ordering
dirCmpr _ _ _ DirectoryItem{diIsDir=False} DirectoryItem{diIsDir=True} = GT
dirCmpr _ _ _ DirectoryItem{diIsDir=True} DirectoryItem{diIsDir=False} = LT
dirCmpr f _ Ascending l@DirectoryItem{diIsDir=False} r@DirectoryItem{diIsDir=False} = compare (f l) (f r)
dirCmpr f _ Descending l@DirectoryItem{diIsDir=False} r@DirectoryItem{diIsDir=False} = compare (f r) (f l)
dirCmpr _ f Ascending  l r = compare (f l) (f r)
dirCmpr _ f Descending l r = compare (f r) (f l)

sortDirectoryItems :: DirectoryItemColumn -> SortMode -> DirectoryItems -> IO ()
sortDirectoryItems DiName sm = A.sortBy (dirCmpr diName diName sm)
sortDirectoryItems DiExt  sm = A.sortBy (dirCmpr diExt  diName sm)
sortDirectoryItems DiSize sm = A.sortBy (dirCmpr diSize diName sm)
sortDirectoryItems DiTime sm = A.sortBy (dirCmpr diTime diTime sm)

size2T :: Integer -> T.Text
size2T = T.pack . go (0 :: Int) ""
    where go _ s 0 = if null s then "0" else s
          go i s n = let (d,m) = divMod n 10
                         c = intToDigit $ fromIntegral m
                     in if i == 3 then go 0 (c:' ':s) d
                        else go (i+1) (c:s) d


time2T :: String -> LocalTime -> T.Text
time2T frmt = T.pack . formatTime defaultTimeLocale frmt

loadDirectoryItems :: [String] -> FilePath -> IO DirectoryItems
loadDirectoryItems exts path = do
    tz <- getCurrentTimeZone -- НЕ вызывать при перенастроенном setForeignEncoding
    let allExts = null exts || ".*" `elem` exts
        exts' = map (map toLower) exts
        removeDot ('.':xs) = xs
        removeDot xs = xs
        extIsAccept = (`elem` exts') . map toLower
        toItem fname = do
            let fullP = path </> fname
            isDir <- doesDirectoryExist fullP
            sz <- if isDir then return 0 else getFileSize fullP
            t <- utcToLocalTime tz <$> getModificationTime fullP
            let (n,e) = if isDir then (fname,"")
                        else let ne@(n',_) = splitExtension fname in
                             if null n' then (fname,"") else ne
            if isDir || allExts || extIsAccept e then
                return $ Just $ DirectoryItem (T.pack n) (T.pack $removeDot e) sz t isDir
            else return Nothing
    lst <- listDirectory path >>= fmap catMaybes . mapM toItem
    v <- VM.new $ length lst
    mapM_ (uncurry (VM.write v)) $ zip [0..] lst
    return v

{-
tstLoadSaveDialog :: IO ()
tstLoadSaveDialog = do
    curDir <- getCurrentDirectory
    putStrLn curDir
    items <- loadDirectoryItems [".*"] curDir
    sortDirectoryItems DiName Descending items
    let itemsCount = VM.length items
        go i = when (i < itemsCount) $ do
          DirectoryItem{..} <- VM.read items i
          T.putStr diName >> putStr "  " >>
            T.putStr diExt >> putStr "  " >>
            T.putStr (size2T diSize) >> putStr "  " >>
            T.putStr (time2T "%d.%m.%y %R" diTime) >> putStr "  " >> print diIsDir
          go $ i+1
    go 0
-}
