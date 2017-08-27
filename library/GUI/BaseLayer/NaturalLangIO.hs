{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      GUI.BaseLayer.NaturalLangIO
-- Copyright:   (c) 2017 KolodeznyDiver
-- License:     BSD3
-- Maintainer:  KolodeznyDiver <kolodeznydiver@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Модуль обеспечивает загрузку файлов текстовых подстановок. Этот каталог располагается
-- в каталоге ресурсов с подкаталогами __\/I18n\/ru_RU\/*.txt
--
-- Формат файлов включает идентификатор сообщения начинающийся с маленькой буквы и содержащий цифры и буквы ASCII
-- После которого, через пробелы, следует сообщение.
-- Сообщение может переходить на другие строки по следующим правилам:
-- 1. Продолжение должно начинаться хотя бы с одного пробела.
-- 2. если продолжение заходит или почти заходит (находится левее или на 1 колонку правее) своего идентификатора,
-- то строки сливаются вместе через пробел. Лишние пробелы вначале строки удаляются.
-- 3. Если строка начинается правее идентификатора, то она вставляется вся, вместе с пробелами и символом \'\\n\'.


module GUI.BaseLayer.NaturalLangIO(
    loadLangFiles
    ) where

import Data.Monoid
import Control.Monad
import Control.Exception.Safe
import System.IO
import System.FilePath
import System.Directory
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import TextShow (showb)
import GUI.BaseLayer.Depend0.Auxiliaries
import GUI.BaseLayer.Depend1.Logging
import GUI.BaseLayer.Depend1.Resource

loadLangFiles:: GUILog -> -- ^ Журнал приложения для вывода сообщений обошибках.
                String -> -- ^ Каталог с файлами строковых фрагментов.
                IO NaturalStringCollection -- ^ См. "GUI.BaseLayer.Depend1.Resource"
loadLangFiles gLog dir = do
    ed <- try $ listDirectory dir
    case ed of
        Right dirlst -> withUtf8Locale $
            foldM (\t fname -> do
                             en <- try $ loadf $ dir </> fname
                             case en of
                                Right n -> return $ t `HM.union` n
                                Left e -> do
                                    logPutLn gLog $ "Can't read file " <> showb fname <>
                                        " : " <> showb (e :: IOError)
                                    return t)
                  HM.empty $ filter ((".txt"==). map toLower . takeExtension) dirlst
        Left e -> do
            logPutLn gLog $ "Can't access directory " <> showb dir <> " : " <> showb (e :: IOError)
            return HM.empty
  where truncSp = T.dropWhile isSpace
        truncSpEnd = T.dropWhileEnd isSpace
        truncSpCnt t = let (sp,r) = T.span isSpace t in (r,T.length sp)
        loadf fname = withFile fname ReadMode $ \h -> do
                      --  putStrLn $ "loadf  " ++ fname
                        let go row t k v pos = do
                                let addMsg | B.null k = t
                                           | otherwise = HM.insert k v t
                                    go' = go (row+1)
                                    goAdd = go' addMsg
                                isEof <- hIsEOF h
                                if isEof then return addMsg else do
                                    s0 <- truncSpEnd <$> TIO.hGetLine h
                                    case T.uncons s0 of
                                        Just (c0,s) | isSpace c0 ->
                                            if B.null k then go' t k v 0
                                            else let (s3,n) = truncSpCnt s in
                                                 go' t k (T.append v
                                                           (if n <= pos then T.cons ' ' s3
                                                            else T.cons '\n' s0)) pos
                                        Just (c0,s) -> do
                                            let (s1,s2) = T.break isSpace s
                                                ident = T.cons c0 s1
                                                idLn = T.length ident
                                            if isLower c0 && T.any isAlphaNum s1 then
                                                goAdd (B.pack $ T.unpack ident)
                                                      (truncSp s2) idLn
                                            else do
                                                logPutLn gLog $ "File " <> showb fname <>
                                                    "  row " <> showb row <>
                                                    ".   Illegal message identificator " <>
                                                    showb ident
                                                go' t k v 0
                                        _ -> if B.null k then go' t k v 0
                                             else go' t k (T.snoc v '\n') pos
                        go (1::Int) HM.empty B.empty T.empty 0


{-
  where truncSp = dropWhile isSpace
        truncSpEnd = dropWhileEnd isSpace
        truncSpCnt = go (0 :: Int)
           where go n [] = ("",n)
                 go n a@(c:cs) | isSpace c = go (n+1) cs
                               | otherwise = (a,n)
        loadf fname = withFile fname ReadMode $ \h -> do
                        putStrLn $ "loadf  " ++ fname
                        let go row t k v pos = do
                                let addMsg | B.null k = t
                                           | otherwise = HM.insert k v t
                                    go' = go (row+1)
                                    goAdd = go' addMsg
                                isEof <- hIsEOF h
                                if isEof then return addMsg else do
                                    s0 <- truncSpEnd <$> hGetLine h
                                    case uncons s0 of
                                        Just (c0,s) | isSpace c0 ->
                                            if B.null k then go' t k v 0
                                            else let (s3,n) = truncSpCnt s in
                                                 go' t k (T.append v $ T.pack
                                                           (if n <= pos then ' ':s3 else '\n':' ':s3)) pos
                                        Just (c0,s) -> do
                                            let (s1,s2) = break isSpace s
                                                ident = c0:s1
                                                idLn = length ident
                                            if isLower c0 && any isAlphaNum s1 then
                                                goAdd (B.pack ident) (T.pack $ truncSp s2) idLn
                                            else do
                                                logPutLn gLog $ "File " <> showb fname <>
                                                    "  row " <> showb row <>
                                                    ".   Illegal message identificator " <>
                                                    showb ident
                                                go' t k v 0
                                        _ -> if B.null k then go' t k v 0
                                             else go' t k (T.snoc v '\n') pos
                        go (1::Int) HM.empty B.empty T.empty 0
-}