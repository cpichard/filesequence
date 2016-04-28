{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module FrameListTests where

import System.FileSequence.FrameList
import Test.Framework
import Data.List (nub, sort)

-- | Frames are stored and correctly restitued in a FrameList
prop_framesCorrectlyRestitued :: [FrameNumber] -> Bool
prop_framesCorrectlyRestitued frm = 
    let sfl = foldl insertFrame emptyFrameList frm in
      sort (toList sfl) == sort (nub frm) 

-- | We should not find a configuration with 2 intervals having contiguous or overlapping values
-- | [(1,2), (2,3)] is forbidden
-- | [(1,5), (2,3)] is forbidden
-- TODO: as we are using intervals, test the intervals function as well
prop_noOverlappingIntervals :: FrameList -> Bool
prop_noOverlappingIntervals = not.hasOverlap.intervals 
  where hasOverlap [] = False
        hasOverlap (_:[]) = False
        hasOverlap (c:cs) = (snd c) >= (fst $ head cs) || hasOverlap cs

-- | First element of an interval is less or equal than the second one
prop_intervalsCorrectlyOrdered :: FrameList -> Bool
prop_intervalsCorrectlyOrdered fs = all sup sfl
  where sup (a,b) = a <= b
        sfl = intervals fs

-- | Number of missing frames max-min - nbframes
prop_holeSize :: FrameList -> Bool
prop_holeSize fs = nbMissing fs == lastFrame fs - firstFrame fs + 1 - nbFrames fs

