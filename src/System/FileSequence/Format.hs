module System.FileSequence.Format (
    -- * Formating functions
      formatSequence
    , formatAsNukeSequence
    , formatAsRvSequence
) where 

import System.FileSequence

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
