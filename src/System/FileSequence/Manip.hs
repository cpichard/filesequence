
-- | Module FileSequenceManip. 
-- Basic file sequence manipulation on the disk.
module System.FileSequence.Manip where

import System.FileSequence
import System.Directory
import System.Posix.Files
import System.Posix.IO
import Control.Monad 

-- |Remove all frames of the sequence
fileSequenceRemove :: FileSequence -> IO ()
fileSequenceRemove fs_ = 
   mapM_ removeFile (frameList fs_)

-- | Copy a all files of a file sequence to a new directory
-- Returns the newly generated filesequence (TODO maybe ? or error)
fileSequenceCopy :: FileSequence -> FilePath -> IO FileSequence
fileSequenceCopy fs_ path_ = do
    pathSrc <- canonicalizePath $ path fs_
    pathDst <- canonicalizePath path_
    if pathSrc == pathDst
        then return fs_
        else let fsr_ = fs_ {path=pathDst} in
             do mapM_ (uncurry copyIfExist) $ zip (frameList fs_) (frameList fsr_) 
                return fsr_
    -- NOTE : test overwrite ?
    where copyIfExist sf_ df_ = do 
             exist <- doesFileExist sf_
             when exist (copyFile sf_ df_)
          
-- |
fileSequenceMove :: FileSequence -> FilePath -> IO FileSequence
fileSequenceMove fs_ path_ = do
    pathSrc <- canonicalizePath $ path fs_
    pathDst <- canonicalizePath path_
    if pathSrc == pathDst
        then return fs_
        else let fsr_ = fs_ {path=pathDst} in
             do mapM_ (uncurry renameIfExist) $ zip (frameList fs_) (frameList fsr_) 
                return fsr_
    where renameIfExist sf_ df_ = do 
             exist <- doesFileExist sf_
             when exist (renameFile sf_ df_)

-- |
--fileSequenceRename :: FileSequence -> String -> IO FileSequence
--fileSequenceRename fs_ name_ = return fs_


-- |Same functionnality as the touch unix command.
fileSequenceTouch :: FileSequence -> IO FileSequence
fileSequenceTouch fs_ = do 
    mapM_ touch $ frameList fs_ 
    -- TODO : recompute filesequence attributes like padding ?
    return fs_
    where touch f = do exists <- fileExist f
                       if exists
                        then touchFile f
                        --  TODO : get the default permissions of the user
                        else do fd <- createFile f ownerReadMode
                                closeFd fd

