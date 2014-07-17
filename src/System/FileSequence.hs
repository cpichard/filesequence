-- |This module contains several functions to create data structures that encapsulate a sequence of file. 
-- This FileSequence data structure can be created from different kind of inputs:
-- directory, list of files and so on.
-- This module also contains functions to access the properties of a FileSequence, like padding, first frame, last frame. 
-- A file sequence is a set of files sharing a common prefix
-- and suffix. Each files is numbered, and the number is between the prefix and suffix. 
--
-- Here is an example of a sequence of file:
--
-- \/home\/kevin\/toto_0000.obj 
--
-- \/home\/kevin\/toto_0001.obj ...
--
-- \/home\/kevin\/toto_0002.obj ...
--
-- => \/home\/kevin\/toto_%04d.obj 0 2

module System.FileSequence (
    -- * FileSequence structure
    FileSequence(..),
    -- * Creation functions
    -- ** From existing directory or files
    fileSequencesFromPaths,
    fileSequencesFromFiles,
    -- ** From names
    fileSequencesFromList,
    fileSequenceFromName,
    fileSequenceFromPrintfFormat,
    -- * Accessors
    frameName,
    frameRange,
    frameList,
    -- * Utils
    getRecursiveDirs,
) where

import System.Directory
import System.FilePath
import Control.Monad
import Data.List
import Text.Regex.PCRE
import Text.Regex.PCRE.ByteString
import Text.Printf
import System.FileSequence.SparseFrameList
import Data.ByteString.UTF8 (fromString, toString, ByteString)
sepReg :: String
sepReg = "(\\.|_)"

frameSepReg :: String
frameSepReg = sepReg++"?"

extSepReg :: String
extSepReg = "(\\.)"

-- * Regex used to find sequences
fileInSeq :: String
fileInSeq = "(.*?)" ++ frameSepReg ++ "(-?[0-9]+)"++ extSepReg ++ "([a-zA-Z0-9]+)$"

-- * Sequence in printf format
seqInPrintf :: String
seqInPrintf = "(.*?)" ++ frameSepReg ++ "%([0-9]+)d" ++ extSepReg ++ "([a-zA-Z0-9]+)$"

-- * Datatype
-- |File sequence data structure.
--  Stores frame range, name, views, extension, padding length
data FileSequence = FileSequence {
      frames            :: SparseFrameList
    , paddingLength     :: Maybe Int-- ^ Padding = number of digit for a frame ex: 00012 -> 5
    , path              :: FilePath -- ^ Directory of the sequence
    , name              :: FilePath -- ^ Name or prefix of the sequence,
    , ext               :: FilePath -- ^ Extension
    , frameSep          :: String   -- ^ Char used to separate the frame
    , extSep            :: String   -- ^ Char used to separate the extension
    } deriving (Show, Eq)

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

-- |Find all the file sequences inside multiple paths
fileSequencesFromPaths :: [FilePath]        -- ^ List of directories
                       -> IO [FileSequence] -- ^ Sequences of files found
fileSequencesFromPaths paths = do
    canonicPaths <- mapM canonicalizePath (nub paths) -- FIXME nub~O(n2)
    filesFound <- mapM directoryContents canonicPaths
    return $ concatMap fileSequencesFromList filesFound
    where directoryContents dir = do
            -- FIXME : getDirectoryContents is not efficient here, try to use readDirStream
            files <- getDirectoryContents dir
            filterM doesFileExist $ map (combine dir) files

-- |Find the sequences from a list of files on the disk
fileSequencesFromFiles :: [FilePath] -> IO [FileSequence]
fileSequencesFromFiles files = do
  existingFiles <- filterM doesFileExist files
  return $ fileSequencesFromList existingFiles

-- |Returns the file sequences of a list of names
fileSequencesFromList :: [String] -> [FileSequence]
fileSequencesFromList nameList = findseq nameList []
    where findseq (x:xs) found =
            case fileSequenceFromName x of
                Nothing -> findseq xs found
                Just fs -> 
                    let (a,b) = break (sameSequence fs) found in 
                    case b of 
                        []     -> findseq xs (fs:found)
                        (y:ys) -> findseq xs $ mergeSequence y fs : (a++ys)
          findseq [] found = found
          mergeSequence fs1 fs2 = 
                fs1 { frames = addFrame (frames fs1) (firstFrame (frames fs2))
                    , paddingLength = deducePadding fs1 fs2
                    }
          deducePadding fs1 fs2
            | paddingLength fs1 == paddingLength fs2 = paddingLength fs1
            | otherwise = Nothing

-- |Return a FileSequence if the name follows the convention
fileSequenceFromName :: String -> Maybe FileSequence
fileSequenceFromName name_ =
    case regResult of
        [[ _ , fullName, sep1, num, sep2, ext_ ]]
            -> Just  FileSequence  
                        { frames = addFrame [] frameNo
                        , paddingLength = deducePadding 
                        , path = path_
                        , name = toString fullName
                        , ext = toString ext_
                        , frameSep = toString sep1
                        , extSep = toString sep2 
                        } 
                where frameNo = read (toString num) :: Int
                      deducePadding
                            | frameNo >= 0 = Just (length (toString num))
                            | otherwise = Just $ length (toString num) - 1
        _   -> Nothing

    where (path_, filename) = splitFileName name_
          regResult = (fromString filename) =~ (fromString fileInSeq) :: [[ByteString]]

-- |Decode the filesequence from a printf format and the first and last frames 
fileSequenceFromPrintfFormat :: String -> Int -> Int -> Maybe FileSequence
fileSequenceFromPrintfFormat name_ ff lf = 
    case regResult of
      [[ _, fullName, sep1, code, sep2, ext_ ]]
          -> Just FileSequence
                    { frames = [((min ff lf), (max ff lf))]
                    , paddingLength = Just (read code :: Int) -- FIXME %d should be Nothing
                    , path = path_
                    , name = fullName
                    , ext = ext_
                    , frameSep = sep1
                    , extSep = sep2
                    }
      _ -> Nothing

    where (path_, filename) = splitFileName name_
          regResult = filename =~ seqInPrintf :: [[String]]

-- |Returns the filename of the frame number
frameName :: FileSequence -> Int -> FilePath
frameName fs_ frame_ = joinPath [path fs_, name fs_ ++ frameSep fs_ ++ frameNumber ++ extSep fs_ ++ ext fs_]
    where frameNumber = printf (padNumber frame_ (paddingLength fs_)) frame_

-- |Returns the list of frames numbers
frameRange :: FileSequence -> [Int]
frameRange fs_ = toList $ frames fs_

-- |Returns the list of all frame names
frameList :: FileSequence -> [FilePath]
frameList fs_ = map (frameName fs_) (frameRange fs_)

-- |Returns the string of the printf format for padded string
--  account for negative frames
--  TODO : move to frameName as it is only used there now
padNumber :: Int -> Maybe Int -> String
padNumber _ Nothing = "%d"
padNumber f (Just pad_) | f >=0 = "%0" ++ show pad_ ++ "d"
                        | otherwise = "%0" ++ show (pad_+1) ++ "d"

-- | From Real world haskell
getRecursiveDirs :: FilePath -> IO [FilePath]
getRecursiveDirs topdir = do
    -- FIXME : getDirectoryContents is not efficient here, try to use readDirStream
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name_ -> do
    let path_ = topdir </> name_
    isDirectory <- doesDirectoryExist path_
    if isDirectory
      then getRecursiveDirs path_
      else return []
  return $ topdir:concat paths

