{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where
import System.FileSequence
import System.FileSequence.SparseFrameList
import Data.List (nub, sort)
import Test.Framework

-- | Check that the padding is not less than the max number of digits in all frames
--paddingIsCoherent :: FileSequence -> Bool
--paddingIsCoherent fs = 
--  case paddingLength fs of
--     Nothing -> True -- differs countDigits
--     Just p -> True -- maximum countDigits >= p
--  where countDigits = map (length.show.abs) (frameRange fs)
--        differs (x:xs) = not $ all (==x) xs
--        differs [] = False


-- | Negative frames quick test
test_negativeFrames :: IO ()
test_negativeFrames = assertEqual a b
    where b = fileSequencesFromList ["test.-0004.dpx", "test.-0003.dpx"]
          a =[FileSequence 
                { frames = [(-4,-3)]
                , paddingLength = Just 4 
                , path = ""
                , name = "test"
                , ext = "dpx"
                , frameSep = "."
                , extSep = "."}] 

-- | Sequence with no name, only numbers
test_sequenceWithoutName :: IO ()
test_sequenceWithoutName = assertEqual a b 
    where b = fileSequenceFromName "0005.dpx"
          a = Just FileSequence 
                    { frames = [(5,5)]
                    , paddingLength = Just 4
                    , path = ""
                    , name = ""
                    , ext = "dpx"
                    , frameSep = ""
                    , extSep = "."}

-- | Test utf8 characters
test_utf8 :: IO ()
test_utf8 = assertEqual a b
    where b = fileSequenceFromName "fffèè.0003.fg"
          a = Just FileSequence 
                    { frames = [(3,3)]
                    , paddingLength = Just 4
                    , path = ""
                    , name = "fffèè"
                    , ext = "fg"
                    , frameSep = "."
                    , extSep = "."}


-- | Frames are restitued correctly in a sparse frame sequence
prop_sparseFrameList :: [Int] -> Bool
prop_sparseFrameList frm = 
    let sfl = foldl addFrame [] frm in
      sort (toList sfl) == sort (nub frm) 

-- | SparseFrameSequence test - first element of a frame range is
-- less or equal than the second one
prop_FrameRange :: FileSequence -> Bool
prop_FrameRange fs = all sup sfl
  where sup (a,b) = a <= b
        sfl = frames fs

-- | Property:
-- list of files A -> FileSequence -> list of files B
-- => A == B 
-- Also enforce that the list of file is not empty
prop_frameConsistency :: FileSequence -> Bool
prop_frameConsistency fs =  sort (frameList fs) == sort (frameList newfss)
  where newfss = head $ fileSequencesFromList (frameList fs)

-- | FIXME Test filesequence equality here 
prop_bijectiveFunc :: FileSequence -> Bool
prop_bijectiveFunc fs =  sort (frameList fs1) == sort (frameList fs2)
  where fs1 = head $ fileSequencesFromList (frameList fs)
        fs2 = head $ fileSequencesFromList (frameList fs1)


-- |Number of missing frames max-min - nbframes
prop_holeSize :: SparseFrameList -> Bool
prop_holeSize [] = nbMissing [] == nbFrames [] -- no frames at all 
prop_holeSize fs = nbMissing fs == lastFrame fs - firstFrame fs + 1 - nbFrames fs



main ::IO ()
main = htfMain htf_thisModulesTests
