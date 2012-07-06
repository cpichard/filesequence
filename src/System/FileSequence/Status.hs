module System.FileSequence.Status where

import System.FileSequence
import System.Posix.Files
import System.Posix.Types
import Control.Monad
import Data.List

-- | A file permission
data Permissions = Permissions {
      ownerReadPerm  :: Bool
    , ownerWritePerm :: Bool
    , ownerExecPerm  :: Bool
    , groupReadPerm  :: Bool
    , groupWritePerm :: Bool
    , groupExecPerm  :: Bool
    , otherReadPerm  :: Bool
    , otherWritePerm :: Bool
    , otherExecPerm  :: Bool
} deriving (Eq, Show)

-- |Returns permissions from a FileStatus info
permissionsFromFileStatus :: FileStatus -> Permissions
permissionsFromFileStatus fs = Permissions owrp owwp owep grrp grwp grep otrp otwp otep
    where hasPerm x = intersectFileModes (fileMode fs) x == x
          owrp = hasPerm ownerReadMode
          owwp = hasPerm ownerWriteMode
          owep = hasPerm ownerExecuteMode
          grrp = hasPerm groupReadMode
          grwp = hasPerm groupWriteMode
          grep = hasPerm groupExecuteMode
          otrp = hasPerm otherReadMode
          otwp = hasPerm otherWriteMode
          otep = hasPerm otherExecuteMode 
        
-- | Structure to store relevant file sequence status infos
data FileSequenceStatus = FileSequenceStatus {
      perms :: [Permissions]
    -- other infos will be stored here !
} deriving Show

-- Returns the status of a FileSequence
getFileSequenceStatus :: FileSequence -> IO FileSequenceStatus
getFileSequenceStatus fs_ = do
    status <- mapM getFileStatus (frameList fs_)
    return $ FileSequenceStatus (perms_ status)
    where perms_ status= nubBy (==) $ map permissionsFromFileStatus status
          

 
