{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.FileSequence.Internal where

import Foreign
import Foreign.C

import System.Posix.FilePath
import Control.Applicative
import Control.Monad (forM, when)
--import System.Posix.Files.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Posix.Files.ByteString
import System.Posix.Directory.Traversals
import Test.QuickCheck
import System.Posix.IO.ByteString 
import Control.Exception
import System.IO.Error

-- |FileSequence path type
-- |This is the type we want to process the path information
type PathString = RawFilePath

-- |Genererate arbitrary PathString for quickcheck
instance Arbitrary PathString where
   arbitrary = BC.pack <$> listOf arbitrary

--instance IsString PathString where
--  fromString g = BC.unpack g
-- duplicates:
-- instance IsString BC.ByteString
-- Defined in ‘Data.ByteString.Internal’

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

-- |Wrapper over posix realpath
realPath :: PathString -> IO PathString
realPath p = do
  BC.useAsCString p $ \pIn ->
    allocaBytes long_path_size $ \pOut ->
      do out <- c_realpath pIn pOut
         BC.packCString out

foreign import ccall unsafe "__hscore_long_path_size"
  long_path_size :: Int

foreign import ccall unsafe "realpath" 
  c_realpath :: CString -> CString -> IO CString

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
