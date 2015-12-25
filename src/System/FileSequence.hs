{-# LANGUAGE OverloadedStrings #-}
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
    -- * Padding structure
    Padding(..),
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
    splitNonContiguous, 
    -- * Export internal structures
    PathString,
    splitDirsAndFiles
) where

import System.Posix.FilePath
import System.Posix.Directory.Traversals
import System.Posix.Files.ByteString
import Control.Monad
import Text.Regex.PCRE
import System.FileSequence.FrameList
import Data.ByteString.UTF8 (fromString, toString)
import Data.Maybe (fromJust)
import System.FileSequence.Internal        
import Data.ByteString.Internal
import Data.Maybe (isJust)
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BC

sepReg :: PathString
sepReg = "(\\.|_)"

frameSepReg :: PathString
frameSepReg = concatPathString [sepReg, "?"]

extSepReg :: PathString
extSepReg = "(\\.)"

-- * Regex used to find sequences
fileInSeq :: PathString
fileInSeq = concatPathString ["(.*?)", frameSepReg, "((-?)([0-9]+))", extSepReg, "([a-zA-Z0-9]+)$"]

-- * Sequence in printf format
seqInPrintf :: PathString
seqInPrintf = concatPathString ["(.*?)", frameSepReg, "%([0-9]+)d", extSepReg, "([a-zA-Z0-9]+)$"]

-- |Padding information 
-- We consider 2 cases:
--    * PaddingFixed:
--      when the padding is fixed and we know it because the number is prefixed with zeroes
--      like 010, 00010, etc.
--    * PaddingMax:
--      when we are not sure the padding is fixed
--      like 677 100 400
--      however it can't be superior to the number of digits
data Padding = PaddingFixed Int
             | PaddingMax Int
             deriving (Show, Eq)

-- |Paddings can be merged if they are compatible.
-- To be compatible, they should be able to restitute the frame string
--   with a printf("%0xd") 
mergePadding :: Padding -> Padding -> Maybe Padding
mergePadding (PaddingFixed a) (PaddingFixed b)
        | a == b = Just $ PaddingFixed a -- same fixed padding
        | otherwise = Nothing            -- different padding ex: 010 != 0010
mergePadding (PaddingMax a) (PaddingFixed b) 
        | b <= a = Just $ PaddingFixed b
        | otherwise = Nothing
mergePadding (PaddingFixed a) (PaddingMax b) = mergePadding (PaddingMax b) (PaddingFixed a)
mergePadding (PaddingMax a) (PaddingMax b) = Just $ PaddingMax (min a b)

-- * Datatype
-- |File sequence data structure.
-- Stores frame range, name, views, extension, padding length
data FileSequence = FileSequence {
      frames            :: FrameList
    , padding           :: Padding    -- ^ Padding = number of digit for a frame ex: 00012 -> 5
    , path              :: PathString -- ^ Directory of the sequence
    , name              :: PathString -- ^ Name or prefix of the sequence,
    , ext               :: PathString -- ^ Extension
    , frameSep          :: PathString -- ^ Char used to isolate the frame from the name
    , extSep            :: PathString -- ^ Char used to separate the extension
    } deriving (Show, Eq)

-- |Returns true if two sequences have the same signature.
-- Two filesequences have the same signature if
-- they share the same name, path, ext, step
sameSignature :: FileSequence -> FileSequence -> Bool
sameSignature fs1 fs2 =
       name fs1 == name fs2
    && path fs1 == path fs2
    && ext  fs1 == ext  fs2
    && frameSep fs1 == frameSep fs2
    && extSep fs1 == extSep fs2

-- |Find all the file sequences inside multiple directory
fileSequencesFromPaths :: [PathString]        -- ^ List of directories
                       -> IO [FileSequence]   -- ^ Sequences of files found
fileSequencesFromPaths paths = do
    canonicPaths <- mapM realpath paths 
    filesFound <- mapM directoryContents canonicPaths
    -- filesFound <- mapM directoryContents paths
    return $ concatMap fileSequencesFromList filesFound
    where directoryContents dir = do
            files <- getDirectoryContents dir
            filterM fileExist $ map (combine dir.snd) files

-- |Find the sequences from a list of files on the disk
fileSequencesFromFiles :: [PathString] -> IO [FileSequence]
fileSequencesFromFiles files = do
  existingFiles <- filterM fileExist files
  return $ fileSequencesFromList existingFiles

-- |Returns the file sequences of a list of file names
fileSequencesFromList :: [PathString] -> [FileSequence]
fileSequencesFromList nameList = findseq nameList []
    where findseq (x:xs) found = 
            case fileSequenceFromName x of
                Nothing -> findseq xs found
                Just fs -> 
                    let (a,b) = break (compatibleSequence fs) found in
                    case b of 
                        []     -> findseq xs (fs:found)
                        (y:ys) -> findseq xs $ mergeSequence y fs : (a++ys)
          findseq [] found = found
          mergePaddingFs fs1 fs2 = mergePadding (padding fs1) (padding fs2)
          compatibleSequence fs1 fs2 = (sameSignature fs1 fs2) && (isJust $ mergePaddingFs fs1 fs2) 
          mergeSequence fs1 fs2 = 
                fs1 { frames = insertFrame (frames fs1) (firstFrame (frames fs2))
                    , padding = fromJust $ mergePaddingFs fs1 fs2 
        }

-- |Return a FileSequence if the name follows the convention
fileSequenceFromName :: PathString -> Maybe FileSequence
fileSequenceFromName name_ =
    case regResult of
        [[ _ , fullName, sep1, num, minus, numbers, sep2, ext_ ]]
            -> if frameNo == 0 && minus == "-"
                then Nothing
                else Just FileSequence  
                    { frames = insertFrame emptyFrameList frameNo
                    , padding = deducePadding 
                    , path = path_
                    , name = fullName
                    , ext = ext_
                    , frameSep = sep1
                    , extSep = sep2 
                    } 
                where frameNo = read (toString num) :: Int 
                      numberLength = length (toString numbers)
                      deducePadding | abs frameNo < 10^(numberLength-1) = PaddingFixed numberLength
                                    | otherwise = PaddingMax numberLength
        _   -> Nothing

    where (path_, filename) = dirAndFileName name_
          regResult = filename =~ fileInSeq :: [[ByteString]]

-- |Decode the filesequence from a printf format and the first and last frames 
fileSequenceFromPrintfFormat :: PathString -> Int -> Int -> Maybe FileSequence
fileSequenceFromPrintfFormat name_ ff lf = 
    case regResult of
      [[ _, fullName, sep1, code, sep2, ext_ ]]
          -> Just FileSequence
                    { frames = fromRange (min ff lf) (max ff lf)
                    , padding = PaddingFixed (read (toString code) :: Int) -- FIXME %d should be Nothing
                    , path = path_
                    , name = fullName
                    , ext = ext_
                    , frameSep = sep1
                    , extSep = sep2
                    }
      _ -> Nothing

    where (path_, filename) = dirAndFileName name_
          regResult = filename =~ seqInPrintf :: [[ByteString]]

-- |Returns the filename of the frame number
frameName :: FileSequence -> Int -> PathString
frameName fs_ frame_ = joinPath [path fs_, 
        concatPathString [name fs_, frameSep fs_, fromString frameNumber, extSep fs_, ext fs_]]
    where frameNumber =
            case padding fs_ of
              PaddingMax _ -> show frame_
              PaddingFixed li -> negStr ++ replicate (nZeros li) '0' ++ absNumber
          absNumber = show $ abs frame_
          negStr = if frame_ < 0
                     then "-"
                     else ""
          nZeros l = l - length absNumber 

-- |Returns the list of frames numbers
frameRange :: FileSequence -> [Int]
frameRange fs_ = toList $ frames fs_

-- |Returns the list of all frame names
frameList :: FileSequence -> [PathString]
frameList fs_ = map (frameName fs_) (frameRange fs_)

-- |Arbitrary FileSequence generator
instance Arbitrary FileSequence where
   arbitrary = do
     frames_ <- listOf1 arbitrary :: Gen [FrameNumber]
     -- Positive len_ <- arbitrary
     plen_ <- elements $ possiblePadding frames_
     frameSep_ <- elements ["", ".", "_"]
     -- FIXME: test won't pass for arbitrary bytestrings, check why
     -- pathName_ <- oneof [arbitrary, elements [BC.pack "/"]]
     -- seqName <- arbitrary `suchThat` nameIsCoherent
     let fs = FileSequence
                { frames = foldl insertFrame emptyFrameList frames_ 
                , padding = plen_
                , path = "/tmp" -- pathName_ 
                , name = "test" -- seqName 
                , ext = "dpx" -- same as above
                , frameSep = frameSep_
                , extSep = "."}
     return fs
     where possiblePadding frms = 
                if differs $ countDigits frms
                  then PaddingMax 1 : digitRange frms
                  else digitRange frms 
           digitRange f = [PaddingFixed x | x <- [maxDigit f .. 2 * maxDigit f]]
           maxDigit = maximum . countDigits
           countDigits = map (length.show.abs) 
           differs (x:xs) = not $ all (==x) xs
           differs [] = True
           --nameIsCoherent x = BC.readInt x == Nothing 
           --                 && all ((flip BC.notElem) x) ['\n', '\0', '\t']

-- |Split a sequence into a list of sequence having contiguous frames (no holes)
splitNonContiguous :: FileSequence -> [FileSequence]
splitNonContiguous fs = map buildSeq $ intervals $ frames fs 
    where buildSeq (ff, lf) = fs {frames = fromRange ff lf}


