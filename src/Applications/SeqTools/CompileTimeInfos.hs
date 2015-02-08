{-# LANGUAGE TemplateHaskell #-}
module CompileTimeInfos where

import CompileTimeInfosTH

-- | Generate a string from the environment variable GITVERSION
-- at compile time
gitVersion :: String
gitVersion = $(lookupVersionEnv)

