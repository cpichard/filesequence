-- |Function to compute the hash of all files in a sequence

module System.FileSequence.Hash where

import qualified Data.ByteString as BS
import System.FileSequence
import System.FileSequence.Internal
import System.IO
import Crypto.Hash
import Control.Monad

-- |First naive implementation of producing a hash from a sequence of files
fileSequenceSum :: FileSequence -> IO String
fileSequenceSum fs = do
    digest <- foldM processFile (hashInit :: Context SHA224) (map pathToString (frameList fs)) 
    let hend = hashFinalize digest
    return $ show hend
    where processFile mdc f = withFile f ReadMode (`hashFile` mdc)
          hashFile h m = do
            eof <- hIsEOF h
            if eof
              then return m
              else do
                chunk <- BS.hGet h 512
                let mdc = hashUpdate m chunk
                mdc `seq` hashFile h mdc

