
module Main ( main ) where
import System.FileSequence
import System.FileSequence.Format
import System.Environment
import System.Console.GetOpt
import System.Directory
--import Data.List

-- seqls command. 
-- Search and display all the sequences in the given path
-- TODO : add search stereo option
-- TODO add recursive search options
-- TODO add status options 
-- TODO add all files as opposed to only sequences
-- TODO add ordering options by size, etc
-- TODO read directories AND list of files or sequences
-- TODO verbose
-- TODO : show permissions with question mark
data OutputFormat = Nuke | Rv | Printf
    deriving Show

detectOutputFormat :: String -> OutputFormat 
detectOutputFormat "rv"     = Rv
detectOutputFormat "printf" = Printf
detectOutputFormat _        = Nuke

-- |Store command line Options
data Options = Options 
    { outputFormat  :: OutputFormat
    , fullPath      :: Bool
    , directoryList :: [String]
    } deriving Show

-- |Default command line options
defaultOptions :: Options
defaultOptions = Options 
    { outputFormat = Nuke
    , fullPath = True
    , directoryList = ["."]
    }

-- |List of options modifiers
options :: [OptDescr (Options -> Options)]
options = 
    [ 
      Option "f" ["format"] 
        (ReqArg (\f opts -> opts {outputFormat=detectOutputFormat f} ) "[nuke|rv|printf]")
         "Format of the output" 
    , Option "g" ["fullpath"]
        (NoArg (\ opt -> opt {fullPath=False}))
        "Display sequence with full path"
    ]

-- | Select the formating function  
formatOutput :: OutputFormat -> Bool -> FileSequence -> String
formatOutput Rv = formatAsRvSequence
formatOutput _  = formatAsNukeSequence

-- |Separate directories from other file types
partitionFiles :: [FilePath] -> IO ([FilePath], [FilePath])
partitionFiles []     = return ([],[])
partitionFiles (x:xs) = do 
  de <- doesDirectoryExist x
  fe <- doesFileExist x
  (yd, yf) <- partitionFiles xs
  return (conc de x yd, conc fe x yf)
  where conc True x' xs' = x':xs'
        conc False _ xs' = xs'  

-- |Process the options modifiers
processOptions :: [Options -> Options] -> [String] -> Options
processOptions optFunc remArgs = addDirectoryList processedOptions remArgs
  where processedOptions = foldl (flip id) defaultOptions optFunc
        addDirectoryList opt_ n_ = 
          case n_ of
            [] -> opt_ -- by default directoryList = "."
            _  -> opt_ { directoryList = n_ }

-- | 
showFoundSequences :: [Options -> Options] -> [String] -> IO ()
showFoundSequences o n = do
  -- debug print opts
  -- partition directories and files
  (directories, files) <- partitionFiles (directoryList opts)
  -- find sequences in directories
  sequencesInDirs <- fileSequencesFromPaths directories
  -- show formatted result
  mapM_ (putStrLn.formatFunc) $ fileSequencesFromList files ++ sequencesInDirs
  where formatFunc = formatOutput (outputFormat opts) (fullPath opts)
        opts = processOptions o n 


-- | Called when an option in the command line is not recognized
showErrorMessage :: IO ()
showErrorMessage = do
  putStrLn $ "unrecognize options"
  putStrLn $ usageInfo "seqls - list file sequences" options

main :: IO ()
main = do 
  args <- getArgs
  let opts = getOpt Permute options args 
  case opts of
    (o, n, [])  -> showFoundSequences o n 
    _           -> showErrorMessage 
    






