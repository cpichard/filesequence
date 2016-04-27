{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module Samples where

import System.FileSequence

-- | Padding extension
extendPaddingSample :: [PathString]
extendPaddingSample = ["b.01.tmp", "b.10.tmp", "b.100.tmp"]

-- | Series of tests with the following case
-- "a sequence can't have the same frame twice"
sameFrameTwice :: [PathString]
sameFrameTwice = ["b.01.tmp", "b.10.tmp", "b.010.tmp"]

