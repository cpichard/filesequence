-- Seqcp : copy sequences of file
-- 
-- ex : seqcp toto.####.exr 1 3 /home/cyril/tmp
module Main where

import System.FileSequence
import System.FileSequence.Manip
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO.Error
import Control.Exception
import Control.Monad

-- |Options for seqcp command line 
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

-- |List of option modifiers
options :: [OptDescr (SeqCpOptions -> SeqCpOptions)]
options = 
    [
        Option "v" ["verbose"]
            (NoArg (\opt -> opt {verbose = True}))
            "Verbose mode"
    ] 

-- |Copy the sequence provided in the options
copySequence :: SeqCpOptions -> IO ()
copySequence cpOpts = do
  case (srcSeq cpOpts, dstPath cpOpts) of
    (Just s, Just p) -> do 
        handle handleExceptAndExit $ void $ fileSequenceCopy s p
        where handleExcept :: IOException -> IO ()
              handleExcept e 
                | isDoesNotExistError e = do 
                    putStrLn $ "error: destination " ++ p ++ " does not exist"
                | isPermissionError e = do
                    putStrLn $ "error: wrong permissions on " ++ p
                | otherwise = do
                    print $ show e
              handleExceptAndExit e = handleExcept e >> exitFailure
    (_, _)  -> do putStrLn "error: missing or wrong arguments."
                  putStrLn "usage: seqcp source_sequence.%05d.txt 1 200 /tmp/dest_dir"
                  exitFailure

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
  args_ <- getArgs
  let cmdlopts = getOpt RequireOrder options args_
  case cmdlopts of
    (o, n, [])  -> copySequence $ processOptions o n
    _           -> showErrorMessage



