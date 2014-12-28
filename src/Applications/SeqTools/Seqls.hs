-- |seqls command.
-- Search and display sequences found in a given path or from a list of files

module Main ( main ) where
import System.FileSequence
import System.FileSequence.Format
import System.FileSequence.Status
import System.FileSequence.SparseFrameList
import System.Environment
import System.Console.GetOpt
import Control.Monad (liftM)
import Data.ByteString.UTF8 (fromString)

-- TODO add full status options
-- TODO add ordering options by size, etc
-- TODO verbose

-- |Seqls datas, comming from the command line arguments
data SeqLsData = SeqLsData
    { outputFormat    :: FormatingOptions
    , pathList        :: [String]
    , recursive       :: Bool
    , minFrames       :: Int
    , contiguous      :: Bool
    } deriving Show

-- |Default seqls datas
defaultOptions :: SeqLsData
defaultOptions = SeqLsData
    { outputFormat = defaultFormatingOptions
    , pathList = ["."]
    , recursive = False
    , minFrames = 1
    , contiguous = False
    }

-- |List of options modifiers
options :: [OptDescr (SeqLsData -> SeqLsData)]
options =
    [
      Option "f" ["format"]
        (ReqArg (updateFormat.setFormatFromString) "[nuke|rv|printf]")
         "Formating style of the output"
    , Option "j" ["min"]
        (ReqArg (\x opt -> opt {minFrames=read x}) "1")
         "Minimal number of frames for a sequence"
    , Option "g" ["fullpath"]
        (NoArg (updateFormat (setFullPath True)))
        "Display sequence with full path"
    , Option "m" ["missing"]
       (NoArg (updateFormat (setMissing True)))
       "Show missing frames"
    , Option "l" ["long"]
       (NoArg (updateFormat (setLongOption True)))
       "Long listing format, provide detailed informations on the sequence"
    , Option "R" ["recursive"]
       (NoArg (\opt -> opt {recursive=True}))
       "Recursive search of file sequences"
    , Option "c" ["contiguous"]
       (NoArg (\opt -> opt {contiguous=True}))
       "Display sequence with contiguous frame only"
    ]
    where updateFormat f opts = opts {outputFormat= f (outputFormat opts)}


-- |Process the options modifiers
processOptions :: [SeqLsData -> SeqLsData] -> [String] -> SeqLsData
processOptions optFunc = addDirectoryList processedOptions
  where processedOptions = foldl (flip id) defaultOptions optFunc
        addDirectoryList opt_ n_ =
          case n_ of
            [] -> opt_ -- by default pathList = "."
            _  -> opt_ { pathList = n_ }

-- |Finally force the printing of the sequences
showFoundSequences :: SeqLsData -> IO ()
showFoundSequences opts = do
  --partition directories and files from command line
  (directories, files) <- splitPaths $ map fromString (pathList opts)
  --in recursive mode add sub directories 
  alldirs <-
      if recursive opts
        then liftM concat (mapM getRecursiveDirs directories)
        else return directories
  --find sequences in directories passed as arguments 
  sequencesInDirs  <- fileSequencesFromPaths alldirs
  --find sequences in files passed as arguments
  sequencesInFiles <- fileSequencesFromFiles files
  --apply filters to select required sequences
  let allSequences = filterMinFrame $ sequencesInFiles ++ sequencesInDirs
      allRequired = if contiguous opts
                      then concatMap splitNonContiguous allSequences
                      else allSequences

  if showStats (outputFormat opts)
    then do
        allRequiredStats <- mapM fileSequenceStatus allRequired
        let zipped = zip allRequired allRequiredStats
        mapM_ (putStrLn.formatWithStats) zipped 
    else
        mapM_ (putStrLn.formatSimple) allRequired 

  --look for sequence extra infos
  -- TODO: when not needed, bypass getStatus as it is taking
  --       a HUGE amount of time 
  -- How lazy works here
  -- allRequiredStats <- mapM fileSequenceStatus allRequired

  -- finally display all sequences
  --mapM_ (putStrLn.format) $ zip allRequired allRequiredStats
  --mapM_ (putStrLn.formatt) allRequired
  where -- formatt = formatSequenceFunction (outputFormat opts)
        formatWithStats = formatResultWithStats (outputFormat opts) 
        formatSimple = formatSimpleResults (outputFormat opts)
        --format = formatResult (outputFormat opts)
        filterMinFrame = filter (\fs -> lastFrame (frames fs) - firstFrame (frames fs) >= minFrames opts - 1)

-- |Called when an option in the command line is not recognized
showErrorMessage :: IO ()
showErrorMessage =
  putStrLn $ usageInfo "seqls - list sequences of files" options

main :: IO ()
main = do
  args <- getArgs
  let cmdlopts = getOpt Permute options args
  case cmdlopts of
    (o, n, [])  -> showFoundSequences (processOptions o n)
    _           -> showErrorMessage


