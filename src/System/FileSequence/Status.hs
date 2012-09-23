module System.FileSequence.Status where

import System.FileSequence
import System.Posix.Files
import System.Posix.Types

-- | A file permission
data Permissions = Permissions
  { ownerReadPerm  :: Maybe Bool
  , ownerWritePerm :: Maybe Bool
  , ownerExecPerm  :: Maybe Bool
  , groupReadPerm  :: Maybe Bool
  , groupWritePerm :: Maybe Bool
  , groupExecPerm  :: Maybe Bool
  , otherReadPerm  :: Maybe Bool
  , otherWritePerm :: Maybe Bool
  , otherExecPerm  :: Maybe Bool
  } deriving (Eq, Show)

-- |Returns permissions from a FileStatus info
permissionsFromFileStatus :: FileStatus -> Permissions
permissionsFromFileStatus fs = Permissions owrp owwp owep grrp grwp grep otrp otwp otep
  where hasPerm x = intersectFileModes (fileMode fs) x == x
        owrp = Just $ hasPerm ownerReadMode
        owwp = Just $ hasPerm ownerWriteMode
        owep = Just $ hasPerm ownerExecuteMode
        grrp = Just $ hasPerm groupReadMode
        grwp = Just $ hasPerm groupWriteMode
        grep = Just $ hasPerm groupExecuteMode
        otrp = Just $ hasPerm otherReadMode
        otwp = Just $ hasPerm otherWriteMode
        otep = Just $ hasPerm otherExecuteMode

-- |Addition of two permission
sumPermissions :: Permissions -> Maybe Permissions -> Permissions
sumPermissions a (Just b) = Permissions owrp owwp owep grrp grwp grep otrp otwp otep
  where hasSamePerm perm_ a_ b_ = if perm_ a_ == perm_ b_ then perm_ a else Nothing
        owrp = hasSamePerm ownerReadPerm  a b
        owwp = hasSamePerm ownerWritePerm a b
        owep = hasSamePerm ownerExecPerm  a b
        grrp = hasSamePerm groupReadPerm  a b
        grwp = hasSamePerm groupWritePerm a b
        grep = hasSamePerm groupExecPerm  a b
        otrp = hasSamePerm otherReadPerm  a b
        otwp = hasSamePerm otherWritePerm a b
        otep = hasSamePerm otherExecPerm  a b
sumPermissions a Nothing = a

-- | Structure to store relevant file sequence informations
data FileSequenceStatus = FileSequenceStatus
  { perms   :: Maybe Permissions      -- ^ Different permissions found for a sequence
  , missing :: [FilePath]       -- ^ List of missing frames
  , maxSize :: FileOffset       -- ^ Max size found in all the frames
  , minSize :: FileOffset       -- ^ Min size found in all the frames
    -- other infos will be stored here !
  } deriving Show

-- |Construct a new file sequence status
newFileSequenceStatus :: FileSequenceStatus
newFileSequenceStatus = FileSequenceStatus Nothing [] minBound maxBound

-- |With the new frame of a filesequence, update the file sequence status data
foldStatus :: FileSequenceStatus -> [FilePath] -> IO FileSequenceStatus
foldStatus fss (x:xs) = do
  isNotMissing <- fileExist x
  if isNotMissing
    then do
      status <- getFileStatus x
      foldStatus (update_ status fss) xs
    else
      foldStatus (missing_ x fss) xs
  where update_ st_ fss_ = fss_
          { perms = Just $ sumPermissions (permissionsFromFileStatus st_) (perms fss_)
          , maxSize = max (fileSize st_) (maxSize fss_)
          , minSize = min (fileSize st_) (minSize fss_)
          }
        missing_ x_ fss_= fss_
          { missing = x_:missing fss_}

foldStatus fss_ [] = return fss_

-- Returns the status of a FileSequence
fileSequenceStatus :: FileSequence -> IO FileSequenceStatus
fileSequenceStatus fs_ = foldStatus newFileSequenceStatus (frameList fs_)






