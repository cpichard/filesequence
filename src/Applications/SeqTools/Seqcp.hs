-- Seqcp : copy sequences of file
-- 
-- ex : seqcp toto.####.exr 1 3 /home/cyril/tmp
module Main where

import System.FileSequence
import System.FileSequence.Format
import System.FileSequence.Status
import System.Environment
import System.Console.GetOpt
import System.Directory
import Control.Monad (liftM)
import System.FilePath.Posix


data SeqCpOptions =
  SeqCpOptions
    { verbose       :: Bool
    , srcSeq        :: Maybe FileSequence
    , dstPath       :: Maybe FilePath
    , args          :: [String]
    }

-- |Returns the default options
defaultOptions :: SeqCpOptions
defaultOptions = 
  SeqCpOptions 
    { verbose = False
    , srcSeq = Nothing
    , dstPath = Nothing 
    , args = []
    }

options :: [OptDescr (SeqCpOptions -> SeqCpOptions)]
options = 
    [
        Option "v" ["verbose"]
            (NoArg (\opt -> opt {verbose = True}))
            "Verbose mode"
    ] 

copySequence :: SeqCpOptions -> IO ()
copySequence cpOpts = do
  case srcSeq cpOpts of
    Nothing -> error "Could not deduce sequence format"
    Just seqn -> do putStrLn "Copying sequence"
                    print seqn
showErrorMessage :: IO ()
showErrorMessage = 
  putStrLn $ usageInfo "seqcp - copy sequence of files" options

-- |Process the command line options
processOptions :: [SeqCpOptions -> SeqCpOptions] -> [String] -> SeqCpOptions
processOptions optMod = readDstPath processedOptions
  where processedOptions = foldl (flip id) defaultOptions optMod
        readDstPath opt_ p_ = 
          case p_ of
            [seqn, ff, lf, dstp] 
                -> opt_ { srcSeq = fileSequenceFromPrintfFormat seqn (read ff :: Int) (read lf :: Int)
                        , dstPath = Just dstp 
                        }
            _ -> opt_



main :: IO ()
main = do
  args <- getArgs
  let cmdlopts = getOpt RequireOrder options args
  case cmdlopts of
    (o, n, [])  -> copySequence $ processOptions o n
    _           -> showErrorMessage



