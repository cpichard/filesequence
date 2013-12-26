module System.FileSequence.Format (
    -- * Formating datas
      FormatingOptions
    , formatSequenceFunction
    , formatResult
    , defaultFormatingOptions
    , setFullPath
    , setFormatFromString
    , setLongOption
    , setMissing
    , splitNonContiguous
) where

import System.FileSequence
import System.FileSequence.Status
import Data.List
import Text.Printf
import Data.Bits

data SequenceFormat = Nuke | Rv | Printf
    deriving Show

-- |Formating options
data FormatingOptions = FormatingOptions
  { sequenceFormat  :: SequenceFormat
  , fullPath        :: Bool
  , showStats       :: Bool
  , showMissing     :: Bool
  } deriving Show

-- |Return the default formating options
defaultFormatingOptions :: FormatingOptions
defaultFormatingOptions = FormatingOptions
  { sequenceFormat = Printf
  , fullPath       = False
  , showStats      = False
  , showMissing    = False
  }

-- |Modify the full path flag in the formating options
setFullPath :: Bool -> FormatingOptions -> FormatingOptions
setFullPath fp opts = opts {fullPath=fp}

-- |Modify the show stats flag (-l) in the formating options
setLongOption :: Bool -> FormatingOptions -> FormatingOptions
setLongOption fp opts = opts {showStats=fp}

-- |Modify the show missing flag in the formating options
setMissing :: Bool -> FormatingOptions -> FormatingOptions
setMissing fp opts = opts {showMissing=fp}

-- |Modify the format of the output sequence
setFormatFromString :: String -> FormatingOptions -> FormatingOptions
setFormatFromString s fo = fo {sequenceFormat = formatFromString s}
  where formatFromString "rv"     = Rv
        formatFromString "printf" = Printf
        formatFromString _        = Nuke

-- |Standard formating for command line
-- Builds a string from the filesequences infos and the formatin options
formatResult :: FormatingOptions
             -> (FileSequence, FileSequenceStatus)
             -> String
formatResult opts =
    \fs -> concatMap ($ fs) layoutFuncs
        where layoutFuncs = intersperse spacefunc showFuncs
              -- build a list of "show" functions depending on a condition
              showFuncs =   consIf (showStats opts) (formatPermFunction opts . snd)
                          $ consIf (showStats opts) (formatSizesFunction opts . snd)
                          $ consIf (showStats opts) (formatFrameFunction opts . fst)
                          $ consIf True (formatSequenceFunction opts . fst)
                          $ consIf showFrameBehind (formatFrameFunction opts . fst)
                          $ consIf (showMissing opts) (formatMissing opts . snd) []
              spacefunc _ = "  " -- Add space between 
              consIf x y = if x then (y:) else id
              showFrameBehind = case sequenceFormat opts of
                                   Rv -> False
                                   _  -> not (showStats opts) 
                                   

-- |Returns the formating function associated to the formating options
formatSequenceFunction :: FormatingOptions -> FileSequence -> String
formatSequenceFunction opts =
   case sequenceFormat opts of
     Rv     -> formatAsRvSequence     (fullPath opts)
     Printf -> formatAsPrintfSequence (fullPath opts)
     _      -> formatAsNukeSequence   (fullPath opts)

-- |Format, reconstruct the sequence name
formatSequence :: FileSequence
               -> (FileSequence -> String)
               -> (FileSequence -> String)
               -> String
formatSequence fs_ path_ padding_ =
    path_ fs_ ++ name fs_ ++ frameSep fs_ ++ padding_ fs_ ++ extSep fs_ ++ ext fs_

-- |Format the structure in a string readable by nuke
formatAsNukeSequence :: Bool -> FileSequence -> String
formatAsNukeSequence fullpath_ fs_ = formatSequence fs_ formatPath formatFrame
    where formatFrame fs = case paddingLength fs of
                            Just pl -> replicate pl '#'
                            Nothing -> "#"
          formatPath
             | fullpath_ = path
             | otherwise = const ""

-- |Format the filesequence as a printf compatible string
formatAsPrintfSequence :: Bool -> FileSequence -> String
formatAsPrintfSequence fullpath_ fs_ = formatSequence fs_ formatPath formatFrame
    where formatFrame fs = case paddingLength fs of 
                             Just pl -> "%0" ++ show pl ++ "d"
                             Nothing -> "%d"
          formatPath
             | fullpath_ = path
             | otherwise = const ""

-- |Format sequence as the output of rvls command
formatAsRvSequence :: Bool -> FileSequence -> String
formatAsRvSequence fullpath_ fs_ = formatSequence fs_ formatPath formatFrame
    where formatFrame fs = show (firstFrame fs) ++ "-" ++ show (lastFrame fs) ++ fixedPadding fs
          fixedPadding fs = case paddingLength fs of
                            Just pl -> replicate pl '@'
                            Nothing -> "#"
          formatPath
             | fullpath_ = path
             | otherwise = const ""

-- |Format the frame section
formatFrameFunction :: FormatingOptions -> FileSequence -> String
formatFrameFunction _ fss = 
    showp (firstFrame fss) ++ " " ++ showp (lastFrame fss)
    where showp n = padBy 8 ' ' (show n)

-- |Format the file size with human readable Kilo Mega and so on 
formatSizesFunction :: FormatingOptions -> FileSequenceStatus -> String
formatSizesFunction _ fss =
    showra (minSize fss) ++ "  " ++ showra (maxSize fss) ++ "  " ++ showra (totSize fss)
    where showra s 
            | shiftR s 10 <= 0 = showp $ show s
            | shiftR s 20 <= 0 = showp $ printf "%4.2fK" (fromIntegral s/1024 :: Float)
            | shiftR s 30 <= 0 = showp $ printf "%4.2fM" (fromIntegral s/(1024*1024) :: Float)
            | shiftR s 40 <= 0 = showp $ printf "%4.2fG" (fromIntegral s/(1024*1024*1024) :: Float)
            | otherwise        = showp $ printf "%4.2fT" (fromIntegral s/(1024*1024*1024*1024) :: Float)
          showp n = padBy 8 ' ' n :: String

-- |Format the file permissions
-- Change the permission to "?" when multiple files have different permissions 
formatPermFunction :: FormatingOptions -> FileSequenceStatus -> String
formatPermFunction _ =
    \fss -> concatMap ($(perms fss)) showFuncs
    where showPerm _  _ Nothing       = "?"
          showPerm pf c (Just perms_) =
             case pf perms_ of
               Nothing   -> "?"
               Just True -> c
               Just False-> "-"
          showFuncs = [ showPerm isSymLink      "l"
                      , showPerm ownerReadPerm  "r"
                      , showPerm ownerWritePerm "w"
                      , showPerm ownerExecPerm  "x"
                      , showPerm groupReadPerm  "r"
                      , showPerm groupWritePerm "w"
                      , showPerm groupExecPerm  "x"
                      , showPerm otherReadPerm  "r"
                      , showPerm otherWritePerm "w"
                      , showPerm otherExecPerm  "x"
                      ]

-- |Format missing frames
formatMissing :: FormatingOptions -> FileSequenceStatus -> String
formatMissing _ fss = 
    "[" ++ showFrames ++ "]"
    where showFrames = intercalate ", " $ map tupleToString (groupContiguousFrames $ sort (missing fss))
          -- Tuple to string : [(1,1), (2,3)] -> ["1", "2-3"]
          tupleToString l | uncurry (==) l = show $ fst l
                          | otherwise      = show (fst l) ++ "-" ++ show (snd l)

-- |Pad a string 
padBy :: Int    -- Pad to a multiple of this number
    -> Char     -- Character used to pad 
    -> String   -- String to pad
    -> String   
padBy n c s = replicate (mod (- length s) n) c ++ s

-- |Returns partitions of contiguous frames
-- ex: [1,2,3,6,7,9] -> [(1,3), (6,7), (9,9]
groupContiguousFrames :: [Int] -> [(Int, Int)]
groupContiguousFrames (x:xs) = grpCon x x xs []
   where grpCon fir las (x':xs') ret = if las+1 == x'
                                        then grpCon fir x' xs' ret
                                        else grpCon x' x' xs' (ret++[(fir, las)])      
         grpCon fir las _ ret = ret ++ [(fir, las)]
groupContiguousFrames _      = []  

-- |Split a sequence into a list of sequence having contiguous frames (no holes)
splitNonContiguous :: FileSequenceStatus -> FileSequence -> [FileSequence]
splitNonContiguous fss fs = map buildSeq $ groupContiguousFrames $ sort $ frameRange fs \\ missing fss
    where buildSeq (ff, lf) = fs {firstFrame=ff, lastFrame=lf}

