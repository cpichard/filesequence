-- |FileSequence module can be use to manage sequence of files.
-- It has basic functionnality used in the vfx industry like handling stereo sequence.
-- Example of file sequences :
-- /home/kevin/toto_0000.obj /home/kevin/toto_0001.obj ...
-- It's supposed the sequence have no holes in it...
-- Later on, there will be a SparseFileSequence

module System.FileSequence (
    -- * FileSequence structure
    FileSequence(..),
    -- * Creation functions
    -- ** From existing directory or files
    fileSequencesFromPath,
    fileSequencesFromPaths,
    fileSequenceFromFile,
    -- ** From names
    fileSequencesFromList,
    fileSequenceFromName,
    -- * Accessors
    frameName,
    frameRange,
    frameList,
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

viewsReg :: String
viewsReg = "(right|left)"

viewSepReg :: String
viewSepReg = "(" ++ sepReg ++ viewsReg ++ ")?"

-- * Regex used to find sequences
fileInSeq :: String
fileInSeq = "(.*?)" ++ viewSepReg ++ frameSepReg ++ "([0-9]+)"++ extSepReg ++ "(.+)"

-- * Datatype
-- |File sequence data structure.
--  Stores frame range, name, views, extension, padding length
data FileSequence = FileSequence {
      firstFrame        :: Int      -- ^ First frame number.
    , lastFrame         :: Int      -- ^ Last frame number.
    , step              :: Int      -- ^ Step between frames. Not used, TODO.
    , paddingLength     :: Maybe Int-- ^ Padding = length of the frame ex: 00012 -> 5
    , path              :: FilePath -- ^ Directory of the sequence
    , name              :: FilePath -- ^ Name of the sequence, 
    , ext               :: FilePath -- ^ Extension
    , viewSep           :: String   -- ^ Char used to separate the view
    , frameSep          :: String   -- ^ Char used to separate the frame
    , extSep            :: String   -- ^ Char used to separate the extension
    , views             :: [String] -- ^ Views of the sequence
    } deriving Show 

-- |Returns true if two sequences have the same signature.
-- |Two filesequences have the same signature if
-- |they share the same name, path, ext, step
sameSequence :: FileSequence -> FileSequence -> Bool
sameSequence fs1 fs2 = 
       name fs1 == name fs2 
    && path fs1 == path fs2
    && ext  fs1 == ext  fs2
    && step fs1 == step fs2
    && viewSep  fs1 == viewSep fs2
    && frameSep fs1 == frameSep fs2
    && extSep fs1 == extSep fs2

-- |Returns a copy of fs1 with an union of fs1 and fs2 frame ranges 
-- |It basically adds frame to a sequence
addFrame :: FileSequence -> FileSequence -> FileSequence
addFrame fs1 fs2 = fs1 { firstFrame=firstF
                       , lastFrame=lastF
                       , views=unionOfViews
                       , paddingLength=deducePadding
                       } 
    where firstF = min (firstFrame fs1) (firstFrame fs2) 
          lastF  = max (lastFrame fs1) (lastFrame fs2)
          unionOfViews = views fs1 `union` views fs2
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

-- |Returns the file sequences of a list of names
fileSequencesFromList :: [String] -> [FileSequence]
fileSequencesFromList nameList = 
    mergeSeq $ groupBy sameSequence potentialSeqs
    where   sort_ = Data.List.sort 
            potentialSeqs = findseq (sort_ nameList) []
            findseq (x:xs) found =  
                case fileSequenceFromName x of
                    Nothing -> findseq xs found
                    Just fs -> findseq xs (found ++ [fs])
            findseq [] found = found
            mergeSeq = map (foldr1 addFrame)

-- |Return eventually a FileSequence if the name follows the convention
fileSequenceFromName :: String -> Maybe FileSequence
fileSequenceFromName name_ =
    case regResult of
        [[   _ , fullName, _, s1, view, s2, num, s3, ext_ ]]  
            -> Just $ FileSequence (toFloat num) (toFloat num) 0 (Just (length num))  path_ fullName ext_ s1 s2 s3 (storeView view)
        _   -> Nothing

    where (path_, filename) = splitFileName name_ 
          regResult = filename =~ fileInSeq :: [[String]]
          toFloat nn = read nn :: Int
          storeView "" = []
          storeView f  = [f]

-- |Returns the filename of the frame number 
-- TODO views + rename formatSequence as something like reconstruct...
frameName :: FileSequence -> Int -> FilePath
frameName fs_ frame_ = formatSequence fs_ path formatViews formatFrames
    where   formatViews _  = ""
            formatFrames fs = printf (padNumber (paddingLength fs)) frame_
            formatSequence fs path_ view_ padding_ = 
                path_ fs ++ name fs ++ view_ fs ++ frameSep fs ++ padding_ fs ++ extSep fs ++ ext fs


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
