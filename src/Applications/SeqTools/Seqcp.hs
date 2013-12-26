-- Seqcp : copy sequences of file
-- 
-- ex : seqcp toto.%05d.exr 1 3 /home/cyril/tmp
module Main where

import System.FileSequence
import System.FileSequence.Manip
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO.Error
import System.IO
import Control.Exception
import Control.Monad

-- TODO verbose mode

-- |Options for seqcp command line 
data SeqCpOptions =
  SeqCpOptions
    { verbose   :: Bool
    , srcSeq    :: Maybe FileSequence
    , dstPath   :: Maybe FilePath
    , args      :: [String]
    , resume    :: Bool
    }

-- |Returns the default options
defaultOptions :: SeqCpOptions
defaultOptions = 
  SeqCpOptions 
    { verbose = False
    , srcSeq = Nothing
    , dstPath = Nothing 
    , args = []
    , resume = False
    }

-- |String prefix in case of an error 
seqcpErrorPrefix :: String
seqcpErrorPrefix = "seqcp error:"

-- |List of option modifiers
options :: [OptDescr (SeqCpOptions -> SeqCpOptions)]
options = 
    [
        Option "v" ["verbose"]
            (NoArg (\opt -> opt {verbose = True}))
            "Verbose mode"
    ,   Option "c" ["resume"]
            (NoArg (\opt -> opt {resume = True}))
            "Continue on error"
    ] 

-- |Copy the sequence provided in the options
copySequence :: SeqCpOptions -> IO ()
copySequence cpOpts = 
  case (srcSeq cpOpts, dstPath cpOpts) of
    (Just s, Just p) -> do 
        if verbose cpOpts
          then putStrLn "I am not in the mood for talking right now"
          else return()
        handle selectHandler $ void $ fileSequenceCopy s p selectHandler
        where selectHandler | resume cpOpts = handleExceptAndResume
                            | otherwise = handleExceptAndExit     
              handleExceptAndExit e = handleExcept e >> exitFailure
              handleExceptAndResume = handleExcept
              handleExcept :: IOException -> IO ()
              handleExcept e 
                | isDoesNotExistError e || isPermissionError e = 
                    let Just filename = ioeGetFileName e in
                    hPutStrLn stderr $ seqcpErrorPrefix ++ " " ++ filename ++ " " ++ ioeGetErrorString e 
                | otherwise = 
                    -- don't know what to print in that case
                    hPutStrLn stderr $ seqcpErrorPrefix ++ show e

    (_, _)  -> do putStrLn "incorrect or missing arguments."
                  exitFailure

-- |Show the help message
showHelpMessage :: IO ()
showHelpMessage = do
  putStrLn $ usageInfo helpMessage options
  where helpMessage = 
         unlines [ "seqcp - copy sequence of files"
                 , "ex: "
                 , " seqcp toto.%05d.exr 1 3 /home/john.doe/tmp"
                 , ""
                 , "options:"
                 ]

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
            [seqn, dstp]
                -> opt_ { srcSeq = fileSequenceFromPrintfFormat seqn 0 0
                        , dstPath = Just dstp
                        }
            _ -> opt_


-- |seqcp executable entry point
main :: IO ()
main = do
  args_ <- getArgs
  let cmdlopts = getOpt RequireOrder options args_
  case cmdlopts of
    (o, n, [])  -> copySequence $ processOptions o n
    _           -> showHelpMessage



