
module Main ( main ) where
import System.FileSequence
import System.FileSequence.Format
import System.Environment
import System.Console.GetOpt

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
detectOutputFormat "rv" = Rv
detectOutputFormat "printf" = Printf
detectOutputFormat _ = Nuke

-- |Store command line Options
data Options = Options {
      outputFormat :: OutputFormat
    , fullPath :: Bool
    , directoryList :: [String]
} deriving Show

defaultOptions :: Options
-- |Default command line options
defaultOptions = Options {
      outputFormat = Nuke
    , fullPath = True
    , directoryList = ["."]
}

-- |List of options modifiers

options :: [OptDescr (Options -> Options)]
options = 
  [ 
      Option "f" ["format"] 
        (ReqArg (\f opts -> opts {outputFormat=detectOutputFormat f} ) "[nuke|rv|printf]")
         "Format ot the output" 
    , Option "g" ["fullpath"]
        (NoArg (\ opt -> opt {fullPath=False}))
        "display sequence with full path"
  ]

-- | Select the formating function  
formatOutput :: OutputFormat -> (Bool -> FileSequence -> String)
formatOutput Rv = formatAsRvSequence
formatOutput _ = formatAsNukeSequence

main :: IO ()
main = do 
    args <- getArgs
    let opts = getOpt Permute options args 
    case opts of
        (o, n, []) -> do
            print fullOption
            sequences <- fileSequencesFromPaths (directoryList fullOption)
            let result = map (formatOutput (outputFormat fullOption) (fullPath fullOption)) sequences
            mapM_ putStrLn result
            where fullOption = addDirectoryList opt n
                  opt = foldl (flip id) defaultOptions o 
                  addDirectoryList opt_ n_ = 
                      case n_ of
                          [] -> opt_
                          _  -> opt_ { directoryList = n_ }

        _ -> do 
            putStrLn $ "unrecognize options"
            putStrLn $ usageInfo "seqls - list file sequences in directories" options
    

