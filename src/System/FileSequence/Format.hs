{-# LANGUAGE OverloadedStrings #-}
-- |Functions to format FileSequence informations to a string, like 'ls' output.   
module System.FileSequence.Format (
    -- * Formating options datas
      FormatingOptions (showStats)
    -- * Formating options data modifiers
    , defaultFormatingOptions
    , setFullPath
    , setLongOption
    , setMissing
    , setFormatFromString
    -- * Format string from FileSequence
    , formatSequenceFunction
    , formatResultWithStats
    , formatSimpleResults
    , formatPermFunction
) where

import System.FileSequence
import System.FileSequence.Internal
import System.FileSequence.Status
import System.FileSequence.SparseFrameList
import Data.List
import Text.Printf
import Data.Bits
import Data.String
import Data.Aeson
import Control.Monad (mzero, liftM)
import Data.ByteString.Lazy.UTF8 (toString)

-- |Styles of sequence formating
data SequenceFormat = Nuke | Rv | Printf | Json
    deriving Show

-- |Formating options
data FormatingOptions = FormatingOptions
  { sequenceFormat  :: SequenceFormat -- ^ Nuke, rv, printf format
  , fullPath        :: Bool           -- ^ show the globall path
  , showStats       :: Bool           -- ^ show additional info, permissions, etc.
  , showMissing     :: Bool           -- ^ show missing frames
  } deriving Show

-- |Return the default formating options, printf and everything to false
defaultFormatingOptions :: FormatingOptions
defaultFormatingOptions = FormatingOptions
  { sequenceFormat = Printf
  , fullPath       = False
  , showStats      = False
  , showMissing    = False
  }

-- |Sets the full path flag in the formating options
setFullPath :: Bool -> FormatingOptions -> FormatingOptions
setFullPath fp opts = opts {fullPath=fp}

-- |Sets the show stats flag (-l) in the formating options
setLongOption :: Bool -> FormatingOptions -> FormatingOptions
setLongOption fp opts = opts {showStats=fp}

-- |Sets the show missing flag in the formating options
setMissing :: Bool -> FormatingOptions -> FormatingOptions
setMissing fp opts = opts {showMissing=fp}

-- |Sets the format of the output sequence
setFormatFromString :: String -> FormatingOptions -> FormatingOptions
setFormatFromString s fo = fo {sequenceFormat = formatFromString s}
  where formatFromString "rv"     = Rv
        formatFromString "printf" = Printf
        formatFromString "json"   = Json
        formatFromString _        = Nuke

-- |Format FileSequence  similarly to ls
-- Builds a string from the FileSequence infos and the formating options
-- FIXME : this function shares the same structure with formatResultWithStats except
-- it doesn't need status information. It is done this way to improve parsing speed
-- by avoiding the use of stat. Is it possible to build only function using lazyness ?
formatSimpleResults :: FormatingOptions
            -> FileSequence
            -> ConsoleString
formatSimpleResults opts = 
    \fs -> concatMap ($ fs) layoutFuncs 
        where layoutFuncs = intersperse spacefunc showFuncs
              -- build a list of "show" functions depending on a condition
              showFuncs =   consIf (showStats opts) (formatFrameFunction opts )
                          $ consIf (showStats opts) (formatNumberOfFrames opts)
                          $ consIf True (formatSequenceFunction opts)
                          $ consIf showFrameBehind (formatFrameFunction opts)
                          $ consIf (showMissing opts) (formatMissing opts) []
              spacefunc _ = "  " -- Add space between 
              consIf x y = if x then (y:) else id
              showFrameBehind = case sequenceFormat opts of
                                   Rv -> False
                                   _  -> not (showStats opts) 

-- |Format FileSequence and status similarly to ls
-- Builds a string from the FileSequence infos and the formating options
formatResultWithStats :: FormatingOptions
             -> (FileSequence, FileSequenceStatus)
             -> ConsoleString
formatResultWithStats opts =
    \fs -> concatMap ($ fs) layoutFuncs
        where layoutFuncs = intersperse spacefunc showFuncs
              -- build a list of "show" functions depending on a condition
              showFuncs =   consIf (showStats opts) (formatPermFunction . snd)
                          $ consIf (showStats opts) (formatSizesFunction opts . snd)
                          $ consIf (showStats opts) (formatFrameFunction opts . fst)
                          $ consIf (showStats opts) (formatNumberOfFrames opts . fst)
                          $ consIf True (formatSequenceFunction opts . fst)
                          $ consIf showFrameBehind (formatFrameFunction opts . fst)
                          $ consIf (showMissing opts) (formatMissing opts . fst) []
              spacefunc _ = "  " -- Add space between 
              consIf x y = if x then (y:) else id
              showFrameBehind = case sequenceFormat opts of
                                   Rv -> False
                                   _  -> not (showStats opts) 

-- |Returns the formating function associated to the formating options
formatSequenceFunction :: FormatingOptions -> FileSequence -> ConsoleString
formatSequenceFunction opts =
   case sequenceFormat opts of
     Rv     -> pathToConsole . formatAsRvSequence     (fullPath opts)
     Printf -> pathToConsole . formatAsPrintfSequence (fullPath opts)
     Json   -> formatAsJson 
     _      -> pathToConsole . formatAsNukeSequence   (fullPath opts)


-- |Format, reconstruct the sequence name
formatSequence :: FileSequence
               -> (FileSequence -> PathString)
               -> (FileSequence -> PathString)
               -> PathString
formatSequence fs_ path_ padding_ =
    concatPathString [path_ fs_, name fs_, frameSep fs_, padding_ fs_, ".", ext fs_]

-- |Format the structure in a string readable by nuke
formatAsNukeSequence :: Bool -> FileSequence -> PathString
formatAsNukeSequence fullpath_ fs_ = formatSequence fs_ formatPath formatFrame
    where formatFrame fs = case padding fs of
                            PaddingFixed pl -> fromString $ replicate pl '#'
                            PaddingMax _ -> "#"
          formatPath
             | fullpath_ = path
             | otherwise = const ""

-- |Format the filesequence as a printf compatible string
formatAsPrintfSequence :: Bool -> FileSequence -> PathString
formatAsPrintfSequence fullpath_ fs_ = formatSequence fs_ formatPath formatFrame
    where formatFrame fs = case padding fs of 
                             PaddingFixed pl -> fromString $ "%0" ++ show pl ++ "d"
                             PaddingMax _ -> "%d"
          formatPath
             | fullpath_ = path
             | otherwise = const ""

-- |Format sequence as the output of rvls command
formatAsRvSequence :: Bool -> FileSequence -> PathString
formatAsRvSequence fullpath_ fs_ = formatSequence fs_ formatPath formatFrame
    where formatFrame fs = fromString $ show (firstFrame (frames fs)) ++ "-" ++ show (lastFrame (frames fs)) ++ fixedPadding fs
          fixedPadding fs = case padding fs of
                            PaddingFixed pl -> fromString $ replicate pl '@'
                            PaddingMax _ -> "#"
          formatPath
             | fullpath_ = path
             | otherwise = const ""

-- |Convert the filesequence to a json representation
formatAsJson ::  FileSequence -> ConsoleString
formatAsJson fs = toString $ encode fs

-- |Format the frame section
formatFrameFunction :: FormatingOptions -> FileSequence -> ConsoleString
formatFrameFunction _ fs = 
    showp (firstFrame (frames fs)) ++ " " ++ showp (lastFrame (frames fs))
    where showp n = padBy 8 ' ' (show n)

-- |Format the file size with human readable Kilo Mega and so on 
formatSizesFunction :: FormatingOptions -> FileSequenceStatus -> ConsoleString
formatSizesFunction _ fss =
    showra (minSize fss) ++ "  " ++ showra (maxSize fss) ++ "  " ++ showra (totSize fss)
    where showra s 
            | shiftR s 10 <= 0 = showp $ show s
            | shiftR s 20 <= 0 = showp $ printf "%4.2fK" (fromIntegral s/1024 :: Float)
            | shiftR s 30 <= 0 = showp $ printf "%4.2fM" (fromIntegral s/(1024*1024) :: Float)
            | shiftR s 40 <= 0 = showp $ printf "%4.2fG" (fromIntegral s/(1024*1024*1024) :: Float)
            | otherwise        = showp $ printf "%4.2fT" (fromIntegral s/(1024*1024*1024*1024) :: Float)
          showp n = padBy 8 ' ' n :: ConsoleString

-- |Format the number of frames
formatNumberOfFrames :: FormatingOptions -> FileSequence -> ConsoleString
formatNumberOfFrames _ fs = (showp $ nbFrames (frames fs)) ++ "  " ++ (showp $ nbMissing (frames fs))
    where showp n = padBy 8 ' ' (show n)

-- |Format the file permissions
-- Change the permission to "?" when multiple files have different permissions 
formatPermFunction :: FileSequenceStatus -> ConsoleString
formatPermFunction =
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
formatMissing :: FormatingOptions -> FileSequence -> ConsoleString
formatMissing _ fs = 
    "[" ++ showFrames ++ "]"
    where showFrames = intercalate ", " $ map tupleToString $ holes (frames fs) 
          -- Tuple to string : [(1,1), (2,3)] -> ["1", "2-3"]
          tupleToString l | uncurry (==) l = show $ fst l
                          | otherwise      = show (fst l) ++ "-" ++ show (snd l)

-- |Format json
--instance FromJSON FileSequence where
--    parseJSON (Object v) = 
--        FileSequence <$> v .: "ranges" 
--                     <*> return (PaddingFixed 1)
--                     <*> (liftM stringToPath (v .: "path"))
--                     <*> (liftM stringToPath (v .: "name"))
--                     <*> (liftM stringToPath (v .: "ext"))   
--                     <*> (liftM stringToPath (v .: "frame_separator"))
--                     <*> (liftM stringToPath (v .: "ext_separator"))
--    parseJSON _ = mzero

-- | FIXME, move to padding file ??
paddingToString p = case p of
        PaddingFixed pl -> "%0" ++ show pl ++ "d"
        PaddingMax _ -> "%d"

instance ToJSON FileSequence where
   toJSON (FileSequence ranges pad path name ext frame_separator) = 
      object [ "padding" .= (paddingToString pad)
             , "ranges" .= ranges 
             , "path" .= (pathToString path)
             , "name" .= (pathToString name)
             , "ext"  .= (pathToString ext)
             , "frame_separator" .= (pathToString frame_separator)] 

instance ToJSON FileSequenceStatus where
    toJSON fss = object [ "perms" .= (formatPermFunction fss)
                        , "size_max" .= show (maxSize fss)
                        , "size_min" .= show (minSize fss)
                        , "size" .= show (totSize fss)
                        ]

-- |Pad a string 
padBy :: Int    -- ^ Pad to a multiple of this number
      -> Char   -- ^ Character used to pad 
      -> String -- ^ String to pad
      -> ConsoleString   
padBy n c s = replicate (mod (- length s) n) c ++ s


