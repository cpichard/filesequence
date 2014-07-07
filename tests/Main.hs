{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Main where
import System.FileSequence
import System.FileSequence.SparseFrameList
import Data.List (nub, sort)
import Test.Framework


-- | Arbitrary FileSequence generator
instance Arbitrary FileSequence where
   arbitrary = do
     frames_ <- listOf1 arbitrary :: Gen [Int]
     Positive len_ <- arbitrary
     plen_ <- elements [Nothing, Just len_]
     frameSep_ <- elements ["", ".", "_"]
     return $ FileSequence
                { frames = foldl addFrame [] frames_ 
                , paddingLength = plen_ 
                , path = "./" --  FIXME : arbitrary for file path
                , name = "test" -- FIXME : arbitrary for names
                , ext = "dpx" -- same as above
                , frameSep = frameSep_
                , extSep = "."} 

-- | Negative frames quick test
test_negativeFrames :: IO ()
test_negativeFrames = 
  do assertEqual a b
    where b = fileSequencesFromList ["test.-0004.dpx", "test.-0003.dpx"]
          a =[FileSequence 
                { frames = [(-4,-3)]
                , paddingLength = Just 5 
                , path = "./"
                , name = "test"
                , ext = "dpx"
                , frameSep = "."
                , extSep = "."}] 

-- | Sequence with no name, only numbers
test_sequenceWithoutName :: IO ()
test_sequenceWithoutName = 
  do assertEqual a b 
    where b = fileSequenceFromName "0005.dpx"
          a = Just (FileSequence 
                    { frames = [(5,5)]
                    , paddingLength = Just 4
                    , path = "./"
                    , name = ""
                    , ext = "dpx"
                    , frameSep = ""
                    , extSep = "."})

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
prop_frameConsistency fs =  sort (frameList fs) == sort (frameList (head newfss))
  where newfss = fileSequencesFromList (frameList fs)

main ::IO ()
main = htfMain htf_thisModulesTests
