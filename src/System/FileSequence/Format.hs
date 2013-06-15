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
) where

import System.FileSequence
import System.FileSequence.Status
import Data.List
import Text.Printf
-- HERE : formating arguments/options
-- remove formatAs functions,
-- add a function to display details on the sequence

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

-- |
setFullPath :: Bool -> FormatingOptions -> FormatingOptions
setFullPath fp opts = opts {fullPath=fp}

setLongOption :: Bool -> FormatingOptions -> FormatingOptions
setLongOption fp opts = opts {showStats=fp}

setMissing :: Bool -> FormatingOptions -> FormatingOptions
setMissing fp opts = opts {showMissing=fp}

-- |
setFormatFromString :: String -> FormatingOptions -> FormatingOptions
setFormatFromString s fo = fo {sequenceFormat = formatFromString s}
  where formatFromString "rv"     = Rv
        formatFromString "printf" = Printf
        formatFromString _        = Nuke

-- |Standard formating for command line
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
                          $ consIf (showMissing opts) (formatMissing opts . snd) []
              spacefunc _ = "  " -- Add space between 
              consIf x y = if x then (y:) else id

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

formatFrameFunction :: FormatingOptions -> FileSequence -> String
formatFrameFunction _ fss = 
    showp (firstFrame fss) ++ showp (lastFrame fss)
    where showp n = pad 6 ' ' (show n)

formatSizesFunction :: FormatingOptions -> FileSequenceStatus -> String
formatSizesFunction _ fss =
    showra (minSize fss) ++ "  " ++ showra (maxSize fss) ++ "  " ++ showra (totSize fss)
    where showra size = showSize (fromIntegral size) ["", "K", "M", "G", "T"]
          showSize :: Float -> [String] -> String
          showSize s (x:xs) = if s < 1024 
                                  then showp $ printf "%4.1f%s" s x
                                  else showSize ff xs 
                              where ff = (s / 1024) :: Float
          showSize s _ = showp (show s) ++ "P"
          showp n = pad 6 ' ' n


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

formatMissing :: FormatingOptions -> FileSequenceStatus -> String
formatMissing _ fss = 
    "[" ++ showFrames ++ "]"
    where showFrames = intercalate ", " $ map tupleToString (grpContiguous frameIntList)
          -- Tuple to string : [(1,1), (2,3)] -> ["1", "2-3"]
          tupleToString l = if fst l == snd l
                              then show $ fst l
                              else show (fst l) ++ "-" ++ show (snd l)
          
          -- Group contiguous values
          grpContiguous (x:xs) = grpCon x x xs []
          grpContiguous _ = []  
          grpCon fir las (x:xs) ret = if las+1 == x
                                        then grpCon fir x xs ret
                                        else grpCon x x xs (ret++[(fir, las)])      
          grpCon fir las _ ret = (ret++[(fir, las)])

          -- List of frame numbers from filenames
          frameIntList = sort $ foldFrames (missing fss) []
          foldFrames (x:xs) fr = case fileSequenceFromName x of
                                   Just fs -> foldFrames xs ((firstFrame fs):fr)
                                   Nothing -> foldFrames xs fr
          foldFrames [] fr = fr
--
--TODO formatUserFunction :: FormatingOptions -> (FileSequenceStatus -> String)
--TODO formatUserFunction _ = (\fss -> "todo")

pad :: Int      -- Size of the returned string
    -> Char     -- Character to add if 
    -> String
    -> String   
pad n c s = replicate (n - length s) c ++ s

