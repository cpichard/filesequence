{-# LANGUAGE TemplateHaskell  #-}

-- | Template helpers to retrieve the versions and other
--  compile time informations
module CompileTimeInfosTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (liftString)
import System.Environment (getEnvironment)
import Control.Monad (liftM)

-- | Get project version via template haskell
lookupVersionEnv :: Q Exp 
lookupVersionEnv = do
  version <- liftM (lookup "GITVERSION") (runIO getEnvironment)
  case version of
    Just v ->  liftString v
    Nothing -> liftString ""
