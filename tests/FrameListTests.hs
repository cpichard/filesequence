{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module FrameListTests where

import System.FileSequence.FrameList
import Test.Framework

-- TODO: as we are using intervals, test the intervals function as well

-- | We should not find a configuration with 2 intervals having contiguous or overlapping values
-- | [(1,2), (2,3)] is forbidden
-- | [(1,5), (2,3)] is forbidden
prop_hasNoContiguousNumber :: FrameList -> Bool
prop_hasNoContiguousNumber = not.hasOverlap.intervals 
  where hasOverlap [] = False
        hasOverlap (_:[]) = False
        hasOverlap (c:cs) = (snd c) >= (fst $ head cs) || hasOverlap cs

-- | Number of missing frames max-min - nbframes
prop_holeSize :: FrameList -> Bool
prop_holeSize fs = nbMissing fs == lastFrame fs - firstFrame fs + 1 - nbFrames fs
