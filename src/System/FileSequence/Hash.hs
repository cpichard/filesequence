{-# LANGUAGE BangPatterns #-}
module System.FileSequence.Hash where

import qualified Data.ByteString as BS
--import System.Directory
import System.FileSequence
import System.IO
import Crypto.Hash
import Control.Monad

-- |First naive implementation of producing a hash from a sequence of files
fileSequenceSum :: FileSequence -> IO String
fileSequenceSum fs = do
    digest <- foldM processFile (hashInit :: Context SHA224) (frameList fs) 
    hend <- return $ hashFinalize digest
    return $ show hend
    where processFile mdc f = withFile f ReadMode $ (\h -> hashFile h mdc)
          hashFile h m = do
            eof <- hIsEOF h
            if eof
              then return m
              else do
                chunk <- BS.hGet h 512
                mdc <- return $ hashUpdate m chunk
                mdc `seq` hashFile h mdc

