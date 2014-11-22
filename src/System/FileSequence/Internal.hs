{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.FileSequence.Internal where


import System.Posix.FilePath
import Control.Applicative
import Control.Monad (forM)
--import System.Posix.Files.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Posix.Files.ByteString
import System.Posix.Directory.Traversals
import Test.QuickCheck

-- FileSequence path type
-- This is the type we want to process the path information
type PathString = RawFilePath

instance Arbitrary PathString where
   arbitrary = BC.pack <$> listOf arbitrary

--instance IsString PathString where
--  fromString g = BC.unpack g

-- Displayed string 
-- This is the type we want to use to display results
type ConsoleString = String

-- |Split filesequence name
splitFSName :: PathString -> (PathString, PathString)
splitFSName x = (BC.reverse b, BC.reverse a)
    where (a,b) = BC.break (=='/') $ BC.reverse x

-- | From Real world haskell
getRecursiveDirs :: PathString -> IO [PathString]
getRecursiveDirs topdir = do
--    -- FIXME : getDirectoryContents is not efficient here, try to use readDirStream
  namesAndTypes <- getDirectoryContents topdir
  let properNames = filter ((`notElem` [".", ".."]).snd) namesAndTypes
  paths <- forM properNames $ \(_,name_) -> do
    let path_ = topdir </> name_
    isDir <- isRawDir path_
    if isDir
      then getRecursiveDirs path_
      else return []
  return $ topdir: concat paths

-- |Split directories from files
splitPaths :: [PathString] -> IO ([PathString], [PathString])
splitPaths []     = return ([],[])
splitPaths (x:xs) = do
  -- isDir <- isDirectory <$> getFileStatus path
  de <- isRawDir x
  --fe <- isRawDir x
  (yd, yf) <- splitPaths xs
  return (conc de x yd, conc (not de) x yf)
  where conc True x' xs' = x':xs'
        conc False _ xs' = xs'

-- Operations on different kind of string used
concatPathString :: [PathString] -> PathString
concatPathString = BS.concat 

concatConsoleString :: [ConsoleString] -> ConsoleString
concatConsoleString = concat

-- Conversion
consoleToPath :: ConsoleString -> PathString
consoleToPath = BC.pack

pathToConsole :: PathString -> ConsoleString
pathToConsole = BC.unpack


pathToString :: PathString -> String
pathToString = BC.unpack

--
isRawDir :: PathString -> IO Bool
isRawDir f = isDirectory <$> getFileStatus f 

