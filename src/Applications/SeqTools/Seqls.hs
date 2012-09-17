
module Main ( main ) where
import System.FileSequence
import System.FileSequence.Format
import System.FileSequence.Status
import System.Environment
import System.Console.GetOpt
import System.Directory

--import Data.List

-- seqls command.
-- Search and display all the sequences in the given path
-- TODO : add search stereo option
-- TODO add full status options
-- TODO add all files as opposed to only sequences
-- TODO add ordering options by size, etc
-- TODO verbose
-- TODO : format as lseq


-- |Seqls datas, comming from the command line arguments
data SeqLsData = SeqLsData
    { outputFormat    :: FormatingOptions
    , pathList        :: [String]
    , stat            :: Bool
    , recursive       :: Bool
    } deriving Show

-- |Default seqls datas
defaultOptions :: SeqLsData
defaultOptions = SeqLsData
    { outputFormat = defaultFormatingOptions
    , pathList = ["."]
    , stat=True
    , recursive=False
    }

-- |List of options modifiers
options :: [OptDescr (SeqLsData -> SeqLsData)]
options =
    [
      Option "f" ["format"]
        (ReqArg  (updateFormat.setFormatFromString)  "[nuke|rv|printf]")
         "Formating style of the output"
    , Option "g" ["fullpath"]
        (NoArg (updateFormat (setFullPath True)) )
        "Display sequence with full path"
    , Option "l" ["long"]
       (NoArg (updateFormat (setLongOption True)))
       "Long listing format, provide detailed informations on the sequence"
    , Option "R" ["recursive"]
       (NoArg (\opt -> opt {recursive=True}))
       "Recursive search of file sequences"
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
processOptions optFunc remArgs = addDirectoryList processedOptions remArgs
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
  alldirs <- do 
      if recursive opts 
        then mapM getRecursiveDirs directories >>= (return . concat)
        else return directories
  -- find sequences in directories
  sequencesInDirs  <- fileSequencesFromPaths alldirs
  -- same for the files
  sequencesOfFiles <- fileSequencesFromFiles files
  -- show formatted result
  let allSequences = sequencesOfFiles ++ sequencesInDirs
  status <- mapM fileSequenceStatus allSequences
  mapM_ (putStrLn.format) (zip allSequences status)
  where format = formatResult (outputFormat opts)

-- |Called when an option in the command line is not recognized
showErrorMessage :: IO ()
showErrorMessage = do
  putStrLn $ usageInfo "seqls - list sequences of files" options

main :: IO ()
main = do
  args <- getArgs
  let cmdlopts = getOpt Permute options args
  case cmdlopts of
    (o, n, [])  -> showFoundSequences (processOptions o n)
    _           -> showErrorMessage


