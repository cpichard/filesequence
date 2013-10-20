
module Main ( main ) where
import System.FileSequence
import System.FileSequence.Format
import System.FileSequence.Status
import System.Environment
import System.Console.GetOpt
import System.Directory
import Control.Monad (liftM)

-- seqls command.
-- Search and display sequences found in a given path or from a list of files
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

-- |Split directories from files
splitPaths :: [FilePath] -> IO ([FilePath], [FilePath])
splitPaths []     = return ([],[])
splitPaths (x:xs) = do
  de <- doesDirectoryExist x
  fe <- doesFileExist x
  (yd, yf) <- splitPaths xs
  return (conc de x yd, conc fe x yf)
  where conc True x' xs' = x':xs'
        conc False _ xs' = xs'

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
  -- partition directories and files
  (directories, files) <- splitPaths (pathList opts)
  
  -- recursive mode ?
  alldirs <-
      if recursive opts
        then liftM concat (mapM getRecursiveDirs directories)
        else return directories
  --find sequences in directories
  sequencesInDirs  <- fileSequencesFromPaths alldirs
  --same for the files
  sequencesOfFiles <- fileSequencesFromFiles files
  --look for sequence extra infos
  -- TODO: when not needed, bypass getStatus as it is taking
  --       a HUGE amount of time 
  let allSequences = filterMinFrame $ sequencesOfFiles ++ sequencesInDirs
  status <- mapM fileSequenceStatus allSequences
  --only contiguous sequences ? -- FIXME do this partition before
  zipped <- if contiguous opts
              then contZipped status allSequences
              else return (zip allSequences status)
  -- finally display all sequences
  mapM_ (putStrLn.format) zipped
  where format = formatResult (outputFormat opts)
        filterMinFrame = filter (\fs -> lastFrame fs - firstFrame fs >= minFrames opts - 1)
        contSeqs st as = concatMap (uncurry splitNonContiguous) $ zip st as
        contZipped st_ as_ = do 
                        contStatus <- mapM fileSequenceStatus (contSeqs st_ as_)
                        return $ zip (contSeqs st_ as_) contStatus

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


