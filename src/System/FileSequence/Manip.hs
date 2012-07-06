
-- | Module FileSequenceManip. 
-- Basic file sequence manipulation on the disk.
module System.FileSequence.Manip where

import System.FileSequence
import System.Directory

-- |Remove all frames of the sequence
fileSequenceRemove :: FileSequence -> IO ()
fileSequenceRemove fs_ = 
   mapM_ removeFile (frameList fs_)

-- | Copy a all files of a file sequence to a new directory
-- Returns the newly generated filesequence (TODO maybe ?)
fileSequenceCopy :: FileSequence -> FilePath -> IO FileSequence
fileSequenceCopy fs_ path_ = return fs_
    --if path fs_ == path_ 
    --    then return fs_ 
    --    else 
    --        -- build the dest FileSequence
    --        --let destFs = fs_ {path=path_}
    --        --map frameName (frameRange fs) fs
    --        return fs_
-- |
fileSequenceMove :: FileSequence -> FilePath -> IO FileSequence
fileSequenceMove fs_ path_ = return fs_

-- |
fileSequenceRename :: FileSequence -> String -> IO FileSequence
fileSequenceRename fs_ name_ = return fs_

-- |
fileSequenceTranslate :: FileSequence -> FileSequence -> IO FileSequence
fileSequenceTranslate fsSrc_ = return 

