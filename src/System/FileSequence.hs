-- |FileSequence module can be use to manage sequence of files.
-- It has basic functionnality used in the vfx industry like handling stereo sequence.
-- Example of file sequences :
-- /home/kevin/toto_0000.obj /home/kevin/toto_0001.obj ...
-- It's supposed the sequence have no holes in it...
-- Later on, there will be a SparseFileSequence
-- NOTE : getDirectoryContents is not efficient here, try to use readDirStream from the
-- System.Posix library

module System.FileSequence (
    -- * FileSequence structure
    FileSequence(..),
    -- * Creation functions
    -- ** From existing directory or files
    fileSequencesFromPath,
    fileSequencesFromPaths,
    fileSequenceFromFile,
    fileSequencesFromFiles,
    -- ** From names
    fileSequencesFromList,
    fileSequenceFromName,
    -- * Accessors
    frameName,
    frameRange,
    frameList,
    -- * Utils
    getRecursiveDirs,
    recursiveDirWalk
) where

import System.Directory
import System.IO.Error
import System.FilePath
import Control.Exception
import Control.Monad
import Data.List
import Text.Regex.PCRE
import Text.Printf

sepReg :: String
sepReg = "(\\.|_)"

frameSepReg :: String
frameSepReg = sepReg

extSepReg :: String
extSepReg = "(\\.)"

-- * Regex used to find sequences
fileInSeq :: String
fileInSeq = "(.*?)" ++ frameSepReg ++ "([0-9]+)"++ extSepReg ++ "([a-zA-Z0-9]+)$"

-- * Datatype
-- |File sequence data structure.
--  Stores frame range, name, views, extension, padding length
data FileSequence = FileSequence {
      firstFrame        :: Int      -- ^ First frame number.
    , lastFrame         :: Int      -- ^ Last frame number.
    , paddingLength     :: Maybe Int-- ^ Padding = number of digit for a frame ex: 00012 -> 5
    , path              :: FilePath -- ^ Directory of the sequence
    , name              :: FilePath -- ^ Name of the sequence,
    , ext               :: FilePath -- ^ Extension
    , frameSep          :: String   -- ^ Char used to separate the frame
    , extSep            :: String   -- ^ Char used to separate the extension
    } deriving Show

-- |Returns true if two sequences have the same signature.
-- |Two filesequences have the same signature if
-- |they share the same name, path, ext, step
sameSequence :: FileSequence -> FileSequence -> Bool
sameSequence fs1 fs2 =
       name fs1 == name fs2
    && path fs1 == path fs2
    && ext  fs1 == ext  fs2
    && frameSep fs1 == frameSep fs2
    && extSep fs1 == extSep fs2

-- |Returns a copy of fs1 with an union of fs1 and fs2 frame ranges
-- |It basically adds frame to a sequence
addFrame :: FileSequence -> FileSequence -> FileSequence
addFrame fs1 fs2 = fs1 { firstFrame = firstF
                       , lastFrame = lastF
                       , paddingLength = deducePadding
                       }
    where firstF = min (firstFrame fs1) (firstFrame fs2)
          lastF  = max (lastFrame fs1) (lastFrame fs2)
          deducePadding = if paddingLength fs1 == paddingLength fs2
                            then paddingLength fs1
                            else Nothing

-- |Find all the file sequences inside path
-- TODO : find a proper way of handling exception here
fileSequencesFromPath :: FilePath -> IO [FileSequence]
fileSequencesFromPath path_ = do
    canonicPath <- canonicalizePath path_
    fileFound <- tryJust (guard . isDoesNotExistError) (getDirectoryContents canonicPath)
    case fileFound of
        Left _          -> return []
        Right fileList  -> return $ fileSequencesFromList (map (combine canonicPath) fileList)

-- |Find all the file sequences inside multiple paths
-- TODO : find a proper way of handling exception here
fileSequencesFromPaths :: [FilePath] -> IO [FileSequence]
fileSequencesFromPaths paths = do
    canonicPaths <- mapM canonicalizePath (nub paths)
    filesFound <- mapM directoryContents canonicPaths
    return $ fileSequencesFromList (concat filesFound)
    where directoryContents dir = do
            files <- getDirectoryContents dir
            return $ map (combine dir) files

-- |Find the sequence the file belongs to.
-- |NOTE : this function can be hugely optimized as it searches all the
-- | sequences in the path instead of comparing the filename.
-- | it leads to something not logical : (x:_)  -> return $ Just x
fileSequenceFromFile :: FilePath -> IO (Maybe FileSequence)
fileSequenceFromFile file =
    case fileSequenceFromName file of
        Just fs -> do
            fsInPath <- fileSequencesFromPath (path fs)
            case matchFs fsInPath [fs] of
                []      -> return Nothing
                (x:_)  -> return $ Just x
                where matchFs = intersectBy sameSequence
        Nothing -> return Nothing

-- |Find the sequences from a list of files on the disk
-- | TODO : handle exceptions here
fileSequencesFromFiles :: [FilePath] -> IO [FileSequence]
fileSequencesFromFiles files = do
  existingFiles <- filterM doesFileExist files
  return $ fileSequencesFromList existingFiles

-- |Returns the file sequences of a list of names
-- fileSequencesFromList :: [String] -> [FileSequence]
-- fileSequencesFromList nameList =
--    mergeSeq $ groupBy sameSequence (sort potentialSeqs)
--    where   potentialSeqs = findseq nameList []
--            findseq (x:xs) found =
--                case fileSequenceFromName x of
--                    Nothing -> findseq xs found
--                    Just fs -> findseq xs (fs:found)
--            findseq [] found = found
--            mergeSeq = map (foldr1 addFrame)

-- |Returns the file sequences of a list of names
fileSequencesFromList :: [String] -> [FileSequence]
fileSequencesFromList nameList = findseq nameList []
    where   findseq (x:xs) found =
                case fileSequenceFromName x of
                    Nothing -> findseq xs found
                    Just fs -> 
                        let (a,b) = break (sameSequence fs) found in 
                        case b of 
                          []     -> findseq xs (fs:found)
                          (y:ys) -> findseq xs $ (addFrame y fs):(a++ys)
            findseq [] found = found

-- |Return a FileSequence if the name follows the convention
fileSequenceFromName :: String -> Maybe FileSequence
fileSequenceFromName name_ =
    case regResult of
        [[ _ , fullName, sep1, num, sep2, ext_ ]]
            -> Just $ FileSequence { firstFrame = (toFloat num)
                                   , lastFrame = (toFloat num)
                                   , paddingLength = (Just (length num))
                                   , path = path_
                                   , name = fullName
                                   , ext = ext_
                                   , frameSep = sep1
                                   , extSep = sep2 
                                   } 
        _   -> Nothing

    where (path_, filename) = splitFileName name_
          regResult = filename =~ fileInSeq :: [[String]]
          toFloat nn = read nn :: Int

-- |Returns the filename of the frame number
frameName :: FileSequence -> Int -> FilePath
frameName fs_ frame_ = formatSequence fs_ path formatFrames
    where   formatFrames fs = printf (padNumber (paddingLength fs)) frame_
            formatSequence fs path_ padding_ =
                path_ fs ++ name fs ++ frameSep fs ++ padding_ fs ++ extSep fs ++ ext fs

-- |Returns the list of frames numbers
frameRange :: FileSequence -> [Int]
frameRange fs_ = [firstFrame fs_ .. lastFrame fs_]

-- |Returns the list of all frame names
frameList :: FileSequence -> [FilePath]
frameList fs_ = map (frameName fs_) (frameRange fs_)

-- |Returns the string of the printf format for padded string
padNumber :: Maybe Int -> String
padNumber (Just pad_) = "%0" ++ show pad_ ++ "d"
padNumber Nothing = "%d"

-- |Real world haskell
getRecursiveDirs :: FilePath -> IO [FilePath]
getRecursiveDirs topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name_ -> do
    let path_ = topdir </> name_
    isDirectory <- doesDirectoryExist path_
    if isDirectory
      then getRecursiveDirs path_
      else return []
  return $ topdir:concat paths

recursiveDirWalk :: ( [FilePath] -> IO () ) -> FilePath -> IO ()
recursiveDirWalk func topdir = do
  names <- getDirectoryContents topdir
  func names
  let properNames = filter (`notElem` [".", ".."]) names
  forM_ properNames $ \name_ -> do
    let path_ = topdir </> name_
    isDirectory <- doesDirectoryExist path_
    if isDirectory
      then recursiveDirWalk func path_ 
      else return ()
  return ()
