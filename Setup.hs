import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.Environment
import System.Process

main = defaultMainWithHooks simpleUserHooks { preBuild = preBuildHook }

-- | Pre build hook to pass the git revision to the program
preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildHook arg bflags = do
  gitversion <- readProcess "git" ["describe","--abbrev=10", "--dirty", "--always", "--tags"] ""
  setEnv "GITVERSION" (filter (/= '\n') gitversion)
  return emptyHookedBuildInfo
