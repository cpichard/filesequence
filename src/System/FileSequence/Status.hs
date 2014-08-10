-- |Functions to extract additional informations like permissions or size from a
-- sequence of files. Is relevant only when the sequence is on the disk, not virtual

module System.FileSequence.Status where

import System.FileSequence
--import System.Posix.Directory.Traversals
--import System.Posix.FilePath
--import System.Posix.Files
import System.Posix.Files.ByteString
--import System.Posix.IO.ByteString
import System.Posix.Types

-- | A file permission
data FileSequenceMode = FileSequenceMode
  { ownerReadPerm  :: Maybe Bool
  , ownerWritePerm :: Maybe Bool
  , ownerExecPerm  :: Maybe Bool
  , groupReadPerm  :: Maybe Bool
  , groupWritePerm :: Maybe Bool
  , groupExecPerm  :: Maybe Bool
  , otherReadPerm  :: Maybe Bool
  , otherWritePerm :: Maybe Bool
  , otherExecPerm  :: Maybe Bool
  , isSymLink      :: Maybe Bool
  } deriving (Eq, Show)

-- |Returns permissions from a FileStatus info
modeFromFileStatus :: FileStatus -> FileSequenceMode
modeFromFileStatus fs = FileSequenceMode owrp owwp owep grrp grwp grep otrp otwp otep syml
  where hasMode x = intersectFileModes (fileMode fs) x == x
        owrp = Just $ hasMode ownerReadMode
        owwp = Just $ hasMode ownerWriteMode
        owep = Just $ hasMode ownerExecuteMode
        grrp = Just $ hasMode groupReadMode
        grwp = Just $ hasMode groupWriteMode
        grep = Just $ hasMode groupExecuteMode
        otrp = Just $ hasMode otherReadMode
        otwp = Just $ hasMode otherWriteMode
        otep = Just $ hasMode otherExecuteMode
        syml = Just $ hasMode symbolicLinkMode 

-- |Addition of two permissions
sumFileSequenceMode :: FileSequenceMode -> Maybe FileSequenceMode -> FileSequenceMode
sumFileSequenceMode a (Just b) = FileSequenceMode owrp owwp owep grrp grwp grep otrp otwp otep syml
  where hasSameMode mode_ a_ b_ = if mode_ a_ == mode_ b_ then mode_ a else Nothing
        owrp = hasSameMode ownerReadPerm  a b
        owwp = hasSameMode ownerWritePerm a b
        owep = hasSameMode ownerExecPerm  a b
        grrp = hasSameMode groupReadPerm  a b
        grwp = hasSameMode groupWritePerm a b
        grep = hasSameMode groupExecPerm  a b
        otrp = hasSameMode otherReadPerm  a b
        otwp = hasSameMode otherWritePerm a b
        otep = hasSameMode otherExecPerm  a b
        syml = hasSameMode isSymLink      a b
sumFileSequenceMode a Nothing = a

-- | Structure to store relevant file sequence informations
data FileSequenceStatus = FileSequenceStatus
  { perms   :: Maybe FileSequenceMode      -- ^ Different permissions found for a sequence
  , maxSize :: FileOffset       -- ^ Max size found in all the frames
  , minSize :: FileOffset       -- ^ Min size found in all the frames
  , totSize :: FileOffset       -- ^ Total size found in all the frames
    -- other infos will be stored here !
  } deriving Show

-- |Construct a new file sequence status
newFileSequenceStatus :: FileSequenceStatus
newFileSequenceStatus = FileSequenceStatus Nothing minBound maxBound 0

-- |With the new frame of a filesequence, update the file sequence status data
foldStatus :: FileSequence -> FileSequenceStatus -> [Int] -> IO FileSequenceStatus
foldStatus fs fss (x:xs) = do
  --isNotMissing <- doesFileExist $ filepath x
  --if isNotMissing
    --then do
      --status <- getSymbolicLinkStatus x
      status <- getFileStatus $ filepath x
      foldStatus fs (update_ status fss) xs
  where filepath = frameName fs 
        update_ st_ fss_ = fss_
          { perms = Just $ sumFileSequenceMode (modeFromFileStatus st_) (perms fss_)
          , maxSize = max (fileSize st_) (maxSize fss_)
          , minSize = min (fileSize st_) (minSize fss_)
          , totSize = fileSize st_ + totSize fss_
          }

foldStatus _ fss_ [] = return fss_

-- |Returns the status of a FileSequence
fileSequenceStatus :: FileSequence -> IO FileSequenceStatus
fileSequenceStatus fs_ = foldStatus fs_ newFileSequenceStatus (frameRange fs_)






