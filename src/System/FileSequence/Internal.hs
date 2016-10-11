{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.FileSequence.Internal where

import Foreign
import System.Posix.FilePath
import System.Posix.Directory.Foreign
import Control.Applicative
import Control.Monad (forM, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import System.Posix.Files.ByteString
import System.Posix.Directory.Traversals
import Test.QuickCheck
import System.Posix.IO.ByteString 
import Control.Exception
import System.IO.Error
import System.IO (hPutStr, stderr)
import Data.ByteString.UTF8 (toString)

--------------------------------------------------------------------------------
-- | Operations on the different kinds of string used
--------------------------------------------------------------------------------

-- | Path string type
-- We use raw bytes to store and process the path information, same as posix
type PathString = RawFilePath

-- | Generate arbitrary PathString for quickcheck testing
instance Arbitrary PathString where
   arbitrary = BC.pack <$> listOf arbitrary

-- Note: we should define
--instance IsString PathString where
--  fromString g = BC.unpack g
-- but it duplicates:
-- instance IsString BC.ByteString
-- Defined in ‘Data.ByteString.Internal’

-- | String type used for display: Unicode haskell String
type ConsoleString = String

-- | Concat path string
concatPathString :: [PathString] -> PathString
concatPathString = BS.concat 

-- | Concat console string
concatConsoleString :: [ConsoleString] -> ConsoleString
concatConsoleString = concat

-- | Convert string used in a console to string used for paths
consoleToPath :: ConsoleString -> PathString
consoleToPath = BC.pack

-- | Convert string used for paths to string used in console
pathToConsole :: PathString -> ConsoleString
pathToConsole = toString

-- | Convert a string used for path to an standard haskell string 
pathToString :: PathString -> String
pathToString = BC.unpack

-- | Test if a path is a directory
isRawDir :: PathString -> IO Bool
isRawDir f = isDirectory <$> getFileStatus f

--------------------------------------------------------------------------------
-- | File system accessories functions
--------------------------------------------------------------------------------

-- | Split filesequence name == dir and base name
dirAndFileName :: PathString -> (PathString, PathString)
dirAndFileName x = (BC.reverse b, BC.reverse a)
    where (a,b) = BC.break (=='/') $ BC.reverse x

-- | Recursive directories (from Real world haskell)
getRecursiveDirs :: PathString -> IO [PathString]
getRecursiveDirs topdir = do
  namesAndTypes <- getDirectoryContents topdir
  let properNames = filter ((`notElem` [".", ".."]).snd) namesAndTypes
  paths <- forM properNames $ \(_,name_) -> do
    let path_ = topdir </> name_
    isDir <- isRawDir path_
    if isDir
      then getRecursiveDirs path_
      else return []
  return $ topdir: concat paths

-- | Traverse folders and apply a function to the list of files contained 
-- in each of them 
visitFolders :: Bool                    -- Recursive
             -> [PathString]            -- Remaining folders
             -> (PathString -> [PathString] -> IO ()) -- Function to apply, takes the current dir and  a list of files
             -> IO ()
visitFolders _ [] _ = return ()
visitFolders recurse (x:xs) func = do
  dirTypesAndNames <- catch (getDirectoryContents x) (handleExcept []) -- Can Throw
  let dots = [".", ".."] :: [PathString]
      properNames = filter (`notElem` dots) (map snd dirTypesAndNames)
  (dirs, files) <- catch (splitDirsAndFiles $ map ( x </> ) properNames) (handleExcept ([],[]))
  -- FIXME : the following is a test with a faster function, it is way faster on local disks but
  -- unfortunately on shared volumes, some sequences are skipped, I still don't know why. 
  --let (dirs, files) = splitDirsAndFilesFast x dirTypesAndNames [] []
  catch (func x files) (handleExcept ())
  let d = if recurse
            then dirs++xs
            else xs
  visitFolders recurse d func
  where handleExcept a e = do
            let err | isPermissionError e  = ": permission error"
                    | isDoesNotExistError e  = ": does not exist"
                    | otherwise = show (e :: IOException)
            hPutStr stderr ( pathToConsole x ++ err ++ "\n")
            return a

-- TODO: change function IO [ConsoleString] to IO ()
--       add more info to the display function
visitFoldersWithDepth :: Bool    -- Recursive
             -> [PathString]     -- Remaining folders
             -> (PathString -> [PathString] -> IO [ConsoleString]) 
             -> ConsoleString -- prefix
             -> IO ()
visitFoldersWithDepth _ [] _ _ = return ()
visitFoldersWithDepth recurse (x:xs) func prefix = do
  -- Read directory
  dirTypesAndNames <- catch (getDirectoryContents x) (handleExcept []) -- Can Throw
  let dots = [".", ".."] :: [PathString]
      properNames = filter (`notElem` dots) (map snd dirTypesAndNames)
  (dirs, files) <- catch (splitDirsAndFiles $ map (x </>)  properNames) (handleExcept ([],[]))
  -- List of formatted sequence
  toDisplay <- catch (func x files) (handleExcept [])
  -- Display directory
  putStrLn $ prefix ++ (lastElmtPrefix xs) ++ pathToConsole x 
  -- Display content
  let newPrefix = if null xs
                    then prefix ++ "   "
                    else prefix ++ "|  "
  displayElement newPrefix (lastElmtPrefix dirs) toDisplay
  -- Display sub directories
  if recurse
    then do 
        visitFoldersWithDepth recurse dirs func newPrefix
    else return ()
  -- Iterate
  visitFoldersWithDepth recurse xs func prefix
  where handleExcept a e = do
            let err | isPermissionError e  = ": permission error"
                    | isDoesNotExistError e  = ": does not exist"
                    | otherwise = show (e :: IOException)
            hPutStr stderr ( pathToConsole x ++ err ++ "\n")
            return a
        displayElement _prefix _ [] = return ()
        displayElement _prefix _seqPx (x:[]) = putStrLn $ _prefix ++ _seqPx ++ x
        displayElement _prefix _seqPx (x:xs) = do 
                putStrLn $ _prefix ++ "|- " ++ x
                displayElement _prefix _seqPx xs
        lastElmtPrefix d = if null d 
                             then "`- "
                             else "|- "
                                                                       
-- |Split files and directories in two lists. This function uses the 
-- standard getStatus function.
splitDirsAndFiles :: [PathString] -> IO ([PathString], [PathString])
splitDirsAndFiles []     = return ([],[])
splitDirsAndFiles (x:xs) = do
  de <- isRawDir x
  (yd, yf) <- splitDirsAndFiles xs
  return (conc de x yd, conc (not de) x yf)
  where conc True x' xs' = x':xs'
        conc False _ xs' = xs'

-- | Split files and directories in two lists. This function uses the
-- direntry information instead of the status to determine if an entry 
-- is a file or a dir.
splitDirsAndFilesFast :: PathString           -- root
                      -> [(DirType, PathString)]  -- list of dirtypes and name returned by getDirectoryContents
                      -> [PathString]             -- directories found (used in internal recursion) 
                      -> [PathString]             -- files found (used in internal recursion)
                      -> ([PathString], [PathString]) -- returned dirs and files
splitDirsAndFilesFast _ [] d f = (d, f)
splitDirsAndFilesFast root (x:xs) d f 
  | fst x == DirType 8 || fst x == DirType 10 || -- FIXME: Finish testing different dirtype on different 
    fst x == DirType 0 || fst x == DirType 14 || -- kind of volumes, filesystems.
    fst x == DirType 2 || fst x == DirType 6  ||
    fst x == DirType 12
     = splitDirsAndFilesFast root xs d (root </> snd x : f)
  | fst x == DirType 4 && (snd x `notElem` [".",".."])
     = splitDirsAndFilesFast root xs (root </> snd x : d) f
  | otherwise = splitDirsAndFilesFast root xs d f


-- | Copy files for internal types
copyFile :: PathString -> PathString -> IO ()
copyFile fromPath toPath =
     copy `catchIOError` (\exc -> throw $ ioeSetLocation exc "copyFile")
     where copy = bracket (openFd fromPath ReadOnly Nothing defaultFileFlags) closeFd $ \fdFrom ->
                  bracketOnError openTmp cleanTmp $ \(fdTmp) ->
                  do allocaBytes bufferSize $ copyContents fdFrom fdTmp
                     ignoreIOExceptions $ copyPermissions fdFrom fdTmp
                     closeFd fdTmp
                     rename tmpFdPath toPath
           tmpFdPath = concatPathString [toPath, ".copyFile.tmp"] -- TODO add pid and remove case
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


