-- |seqsum -  compute the hash of a sequence of files
module Main ( main ) where
import System.FileSequence
import System.FileSequence.Hash
import System.FileSequence.Format
import System.FileSequence.FrameList
import System.Environment
import System.Console.GetOpt
import Control.Monad (liftM)
import Data.ByteString.UTF8 (fromString)

data SeqSumData = SeqSumData
    { pathList        :: [String]
    , recursive       :: Bool
    , minFrames       :: Int
    } deriving Show

-- |Default seqsum datas
defaultOptions :: SeqSumData
defaultOptions = SeqSumData
    { pathList = ["."]
    , recursive = False
    , minFrames = 1
    }

-- |List of options modifiers
options :: [OptDescr (SeqSumData -> SeqSumData)]
options =
    [
      Option "j" ["min"]
        (ReqArg (\x opt -> opt {minFrames=read x}) "1")
         "Minimal number of frames for a sequence"
    , Option "R" ["recursive"]
       (NoArg (\opt -> opt {recursive=True}))
       "Recursive search of file sequences"
    ]

-- |Process the options modifiers
processOptions :: [SeqSumData -> SeqSumData] -> [String] -> SeqSumData
processOptions optFunc = addDirectoryList processedOptions
  where processedOptions = foldl (flip id) defaultOptions optFunc
        addDirectoryList opt_ n_ =
          case n_ of
            [] -> opt_ -- by default pathList = "."
            _  -> opt_ { pathList = n_ }

runSeqSum :: SeqSumData -> IO ()
runSeqSum opts = do
  (directories, files) <- splitDirsAndFiles $ map fromString (pathList opts)
  alldirs <-
      if recursive opts
        then liftM concat (mapM getRecursiveDirs directories)
        else return directories
  sequencesInDirs  <- fileSequencesFromPaths alldirs
  sequencesOfFiles <- fileSequencesFromFiles files
  --show formatted result
  let allSequences = filterMinFrame $ sequencesOfFiles ++ sequencesInDirs
  hashes <- mapM fileSequenceSum allSequences 
  mapM_ (putStrLn.format) (zip allSequences hashes)
  where filterMinFrame = filter (\fs -> lastFrame (frames fs) - firstFrame (frames fs) >= minFrames opts - 1)
        format x = formatSequenceFunction defaultFormatingOptions (fst x) ++ "  " ++ snd x

-- seqsum command
showErrorMessage :: IO ()
showErrorMessage =
  putStrLn $ usageInfo "seqsum - hash sequence of file" options

main :: IO ()
main = do
  args <- getArgs
  let cmdlopts = getOpt Permute options args
  case cmdlopts of
    (o, n, [])  -> runSeqSum (processOptions o n)
    _           -> showErrorMessage
