module System.FileSequence.Format (
    -- * Formating datas
      FormatingOptions
    , formatSequenceFunction
    , formatStatusFunction
    , defaultFormatingOptions
    , setFullPath
    , setFormatFromString
) where

import System.FileSequence
import System.FileSequence.Status

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
  } deriving Show

-- |Return the default formating options
defaultFormatingOptions :: FormatingOptions
defaultFormatingOptions = FormatingOptions
  { sequenceFormat = Rv
  , fullPath       = False
  , showStats      = False
  }

-- |
setFullPath :: Bool -> FormatingOptions -> FormatingOptions
setFullPath fp opts = opts {fullPath=fp}

-- |
setFormatFromString :: String -> FormatingOptions -> FormatingOptions
setFormatFromString s fo = fo {sequenceFormat = (formatFromString s)}
  where formatFromString "rv"     = Rv
        formatFromString "printf" = Printf
        formatFromString _        = Nuke

-- |Returns the formating function associated to the formating options
formatSequenceFunction :: FormatingOptions -> (FileSequence -> String)
formatSequenceFunction opts =
   case sequenceFormat opts of
     Rv -> formatAsRvSequence  (fullPath opts)
     _  -> formatAsNukeSequence (fullPath opts)

-- |Format, reconstruct the sequence name
formatSequence :: FileSequence
               -> (FileSequence -> String)
               -> (FileSequence -> String)
               -> (FileSequence -> String)
               -> String
formatSequence fs_ path_ view_ padding_ =
    path_ fs_ ++ name fs_ ++ view_ fs_ ++ frameSep fs_ ++ padding_ fs_ ++ extSep fs_ ++ ext fs_

-- |Format the structure in a string readable by nuke
formatAsNukeSequence :: Bool -> FileSequence -> String
formatAsNukeSequence fullpath_ fs_ = formatSequence fs_ formatPath formatView formatFrame
    where formatFrame fs = case paddingLength fs of
                            Just pl -> replicate pl '#'
                            Nothing -> "#"
          formatView  fs =
              case views fs of
                  [] -> ""
                  _  -> viewSep fs ++ "%V"
          formatPath
             | fullpath_ = path
             | otherwise = const ""

-- TODO formatAsPrintf


-- |Format sequence as the output of rvls command
formatAsRvSequence :: Bool -> FileSequence -> String
formatAsRvSequence fullpath_ fs_ = formatSequence fs_ formatPath formatView formatFrame
    where formatFrame fs = show (firstFrame fs) ++ "-" ++ show (lastFrame fs) ++ fixedPadding fs
          fixedPadding fs = case paddingLength fs of
                            Just pl -> replicate pl '@'
                            Nothing -> "#"
          formatView  _ = ""
          formatPath
             | fullpath_ = path
             | otherwise = const ""

formatStatusFunction :: FormatingOptions -> (FileSequenceStatus -> String)
formatStatusFunction opt = 
    (\fss -> concat (map ($(fss)) showFuncs))
    where showFuncs = [ formatPermFunction opt
                      , (\_ -> "  ") -- Space function 
                      , formatSizesFunction opt
                      ]

formatSizesFunction :: FormatingOptions -> (FileSequenceStatus -> String)
formatSizesFunction _ = 
    (\fss -> (show $ maxSize fss) ++ "  " ++ (show $ minSize fss))

formatPermFunction :: FormatingOptions -> (FileSequenceStatus -> String)
formatPermFunction _ =
    (\fss -> concat (map ($(perms fss)) showFuncs))
    where showPerm _  _ Nothing       = "?"
          showPerm pf c (Just perms_) = 
             case pf perms_ of
               Nothing   -> "?"
               Just True -> c
               Just False-> "-" 
          showFuncs = [ showPerm ownerReadPerm  "r"
                      , showPerm ownerWritePerm "w"
                      , showPerm ownerExecPerm  "x"
                      , showPerm groupReadPerm  "r"
                      , showPerm groupWritePerm "w"
                      , showPerm groupExecPerm  "x"
                      , showPerm otherReadPerm  "r"
                      , showPerm otherWritePerm "w"
                      , showPerm otherExecPerm  "x"
                      ]
                     
                     
                      


