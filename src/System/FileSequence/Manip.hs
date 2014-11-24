-- |Module FileSequence.Manip,
{-# LANGUAGE Rank2Types #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- provides basic file sequence manipulation on the file system.
module System.FileSequence.Manip ( 
    fileSequenceRemove,
    fileSequenceCopy, 
    fileSequenceMove,
    fileSequenceTouch
) where

import System.FileSequence
import System.FileSequence.Internal
import System.Posix.FilePath
import System.Posix.Directory.Traversals
--import System.Posix.Directory.ByteString
import System.Posix.Files.ByteString
import System.Posix.IO.ByteString
--import qualified Data.ByteString as BS
--import System.Directory
--import System.Posix.Files
--import System.Posix.IO
import System.IO.Error
import Control.Exception

-- |Type synonym for the map frames exception handler
-- require Rank2Types
type ExceptionHandler e = Exception e => (e -> IO ())

-- |Remove all frames of the sequence
fileSequenceRemove :: FileSequence -> IO ()
fileSequenceRemove fs_ = 
   mapM_ removeLink (frameList fs_)

-- |Copy a all files of a file sequence to a new directory
-- Returns the newly generated filesequence
fileSequenceCopy :: Exception e 
                 => FileSequence        -- ^ FileSequence to copy 
                 -> PathString          -- ^ Destination directory
                 -> ExceptionHandler e  -- ^ Exception handler 
                 -> IO FileSequence     -- ^ Created sequence
fileSequenceCopy fs_ path_ hand_ = do
    isDir <- isRawDir path_
    if isDir
      then do 
        pathSrc <- realpath $ path fs_ 
        pathDst <- realpath path_  
        if pathSrc == pathDst
          then return fs_
          else let fsr_ = fs_ {path=pathDst} in do
               mapFrames tryCopyFile fs_ fsr_
               return fsr_ 
       else throw $ mkIOError doesNotExistErrorType "does not exists" Nothing (Just (pathToString path_)) 
    where tryCopyFile a b = handle hand_ $ copyFile a b
          copyFile a b = error "implement copy"-- return () -- FIXME copy file for real

-- |Move all filesequence frames to the new path
fileSequenceMove :: FileSequence -> PathString -> IO FileSequence
fileSequenceMove fs_ path_ = do
    pathSrc <- realpath $ path fs_
    pathDst <- realpath path_ 
    if pathSrc == pathDst
        then return fs_
        else let fsr_ = fs_ {path=pathDst} in do
             mapFrames rename fs_ fsr_
             return fsr_
                  

-- |map function for each frames (fs, fd) of the src and dst sequence
-- src and dst must have the same number of frames
mapFrames :: (PathString -> PathString -> IO ()) -- function to apply
          -> FileSequence -- src frames
          -> FileSequence -- dst frames
          -> IO ()
mapFrames func src_ dst_ = mapM_ (uncurry func) $ zip (frameList src_) (frameList dst_) 


-- |
--fileSequenceRename :: FileSequence -> String -> IO FileSequence
--fileSequenceRename fs_ name_ = return fs_

-- |Same functionnality as the touch unix command.
fileSequenceTouch :: FileSequence -> IO FileSequence
fileSequenceTouch fs_ = do 
    mapM_ touch $ frameList fs_ 
    -- TODO : recompute filesequence attributes like padding ?
    return fs_
    where touch f = do 
            exists <- fileExist f
            if exists
              then touchFile f
              --  TODO : get the default permissions of the user
              else do fd <- createFile f ownerReadMode
                      closeFd fd

