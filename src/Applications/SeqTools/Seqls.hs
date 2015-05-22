-- | seqls command.
-- Search and display sequences found in a given path or from a list of files

module Main ( main ) where
import System.FileSequence
import System.FileSequence.Format
import System.FileSequence.Status
import System.FileSequence.SparseFrameList
import System.FileSequence.Internal
import System.Environment
import System.Console.GetOpt
import qualified Data.ByteString.Char8 as BC
import Control.Monad (liftM)
import Data.ByteString.UTF8 (fromString)
import CompileTimeInfos (gitVersion)

-- TODO add full status options
-- TODO add ordering options by size, etc

-- | Seqls datas, comming from the command line arguments
data SeqLsData = SeqLsData
    { outputFormat    :: FormatingOptions
    , pathList        :: [String]
    , recursive       :: Bool
    , minFrames       :: Int
    , contiguous      :: Bool
    , followLink      :: Bool
    , filterExt       :: [PathString]
    } deriving Show

-- | Default seqls datas
defaultOptions :: SeqLsData
defaultOptions = SeqLsData
    { outputFormat = defaultFormatingOptions
    , pathList = ["."]
    , recursive = False
    , minFrames = 3
    , contiguous = False
    , followLink = True
    , filterExt = []
    }

-- | Extract extensions from a string and copy them to seqlsdata
setFilterExtFromString :: String -> SeqLsData -> SeqLsData
setFilterExtFromString s f = f {filterExt = extListBS}
  where extListBS = BC.split ',' $ consoleToPath s 


-- | List of options modifiers
options :: [OptDescr (SeqLsData -> SeqLsData)]
options =
    [
      Option "f" ["format"]
        (ReqArg (updateFormat.setFormatFromString) "[nuke|rv|printf]")
         "Formating style of the output"
    , Option "j" ["min"]
        (ReqArg (\x opt -> opt {minFrames=read x}) "3")
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
    , Option "s" ["nosymlink"]
       (NoArg (\opt -> opt {followLink=False}))
       "Do not follow symlinks"
    , Option "e" ["ext"]
       (ReqArg setFilterExtFromString "exr,dpx,jpg")
       "Filter by extension, comma separated list of extensions"    
    ]
    where updateFormat f opts = opts {outputFormat= f (outputFormat opts)}


-- | Process the options modifiers
processOptions :: [SeqLsData -> SeqLsData] -> [String] -> SeqLsData
processOptions optFunc = addDirectoryList processedOptions
  where processedOptions = foldl (flip id) defaultOptions optFunc
        addDirectoryList opt_ n_ =
          case n_ of
            [] -> opt_ -- by default pathList = "."
            _  -> opt_ { pathList = n_ }

-- | Extract and display sequences from a list of files 
showSequencesInFiles :: SeqLsData -> [PathString] -> IO ()
showSequencesInFiles _ [] = return () 
showSequencesInFiles opts files = do
  sequencesInFiles <- fileSequencesFromFiles files -- can throw ?
  let allSequences = filterExtension (filterExt opts) $ filterMinFrame $ sequencesInFiles
      allRequired = if contiguous opts
                      then concatMap splitNonContiguous allSequences
                      else allSequences

  if showStats (outputFormat opts)
    then do
        allRequiredStats <- if (followLink opts)
                then mapM fileSequenceStatus allRequired        -- can throw
                else mapM fileSequenceSymlinkStatus allRequired -- can throw
        let zipped = zip allRequired allRequiredStats
        mapM_ (putStrLn.formatWithStats) zipped 
    else
        mapM_ (putStrLn.formatSimple) allRequired 
    
  where formatWithStats = formatResultWithStats (outputFormat opts) 
        formatSimple = formatSimpleResults (outputFormat opts)
        filterMinFrame = filter (\fs -> lastFrame (frames fs) - firstFrame (frames fs) >= minFrames opts - 1)
        filterExtension [] = id
        filterExtension exts = filter (\fs -> (ext fs) `elem` exts)

-- | Finally force the printing of the sequences
runSeqls :: SeqLsData -> IO ()
runSeqls opts = do
  -- partition directories and files from command line
  (folders, files) <- splitPaths $ map fromString (pathList opts)
  -- First display the files
  showSequencesInFiles opts files 
  -- then display each folder one after the other
  visitFolders (recursive opts) folders (showSequencesInFiles opts)

-- | Called when an option in the command line is not recognized
showErrorMessage :: IO ()
showErrorMessage =
  putStrLn $ usageInfo ("seqls version " ++ gitVersion ++ "\nList sequences of files") options

main :: IO ()
main = do
  args <- getArgs
  let cmdlopts = getOpt Permute options args
  case cmdlopts of
    (o, n, [])  -> runSeqls (processOptions o n)
    _           -> showErrorMessage


