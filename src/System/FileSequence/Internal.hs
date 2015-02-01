{-# LANGUAGE TemplateHaskell, ForeignFunctionInterface, OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.FileSequence.Internal where

import Foreign

import System.Posix.FilePath
import Control.Applicative
import Control.Monad (forM, when, liftM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Posix.Files.ByteString
import System.Posix.Directory.Traversals
import Test.QuickCheck
import System.Posix.IO.ByteString 
import Control.Exception
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (liftString)
import System.Environment (getEnvironment)
import System.IO.Error

-- | Get project version via template haskell
lookupVersionEnv :: Q Exp 
lookupVersionEnv = do
  version <- liftM (lookup "GITVERSION") (runIO getEnvironment)
  case version of
    Just v ->  liftString v
    Nothing -> liftString ""


-- |FileSequence path type
-- |We use raw bytes to process the path information
type PathString = RawFilePath

-- |Generate arbitrary PathString for quickcheck testing
instance Arbitrary PathString where
   arbitrary = BC.pack <$> listOf arbitrary

-- Note: we should define
--instance IsString PathString where
--  fromString g = BC.unpack g
-- but it duplicates:
-- instance IsString BC.ByteString
-- Defined in ‘Data.ByteString.Internal’

-- Displayed string 
-- We use UTF8 haskell String to display the results
type ConsoleString = String

-- |Split filesequence name
splitDirectoryAndFile :: PathString -> (PathString, PathString)
splitDirectoryAndFile x = (BC.reverse b, BC.reverse a)
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
  de <- isRawDir x
  (yd, yf) <- splitPaths xs
  return (conc de x yd, conc (not de) x yf)
  where conc True x' xs' = x':xs'
        conc False _ xs' = xs'

-- |Operations on different kind of string used

-- |Concat path string
concatPathString :: [PathString] -> PathString
concatPathString = BS.concat 

-- |Concat console string
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

-- |Copy files for internal types
copyFile :: PathString -> PathString -> IO ()
copyFile fromPath toPath =
     copy `catchIOError` (\exc -> throw $ ioeSetLocation exc "copyFile")
     where copy = bracket (openFd fromPath ReadOnly Nothing defaultFileFlags) closeFd $ \fdFrom ->
                  bracketOnError openTmp cleanTmp $ \(fdTmp) ->
                  do allocaBytes bufferSize $ copyContents fdFrom fdTmp
                     ignoreIOExceptions $ copyPermissions fdFrom fdTmp
                     closeFd fdTmp
                     rename tmpFdPath toPath
           tmpFdPath = concatPathString [toPath, ".copyFile.tmp"]
           openTmp = openFd tmpFdPath WriteOnly (Just 0o600) defaultFileFlags
           cleanTmp fdTmp'
                = do ignoreIOExceptions $ closeFd fdTmp'
                     ignoreIOExceptions $ removeLink tmpFdPath
           bufferSize = 1024 :: Int
           copyContents fdFrom' fdTmp' buffer = do
                    count <- fdReadBuf fdFrom' buffer (fromIntegral bufferSize)
                    when (count >0) $ do 
                            _ <- fdWriteBuf fdTmp' buffer count
                            copyContents fdFrom' fdTmp' buffer
           ignoreIOExceptions io = io `catchIOError` (\_ -> return ())
           copyPermissions fdFrom' fdTmp' = do
              statFrom <- getFdStatus fdFrom'
              setFdMode fdTmp' (fileMode statFrom)
