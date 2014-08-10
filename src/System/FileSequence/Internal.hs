{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module System.FileSequence.Internal where
import System.Posix.FilePath
import Control.Applicative
--import System.Posix.Files.ByteString
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Posix.Files.ByteString
-- FileSequence Path type
type PathString = RawFilePath

--instance IsString PathString where
--  fromString g = BC.unpack g

-- Displayed type of String 
type ConsoleString = String

concatPathString :: [PathString] -> PathString
concatPathString = BS.concat 

concatConsoleString :: [ConsoleString] -> ConsoleString
concatConsoleString = concat


consoleToPath :: ConsoleString -> PathString
consoleToPath = BC.pack

pathToConsole :: PathString -> ConsoleString
pathToConsole = BC.unpack


pathToString :: PathString -> String
pathToString = BC.unpack


isRawDir :: PathString -> IO Bool
isRawDir f = isDirectory <$> getFileStatus f 

