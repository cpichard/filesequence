{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework
import System.FileSequence

-- Negative frames
test_negativeFrames :: IO ()
test_negativeFrames = 
  do assertEqual a b
    where b = fileSequencesFromList ["test.-0004.dpx", "test.-0003.dpx"]
          a =[FileSequence 
                {frames = [(-4,-3)]
                , paddingLength = Just 6
                , path = "./"
                , name = "test"
                , ext = "dpx"
                , frameSep = "."
                , extSep = "."}] 

-- Sequence with no name, only numbers
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
main ::IO ()
main = htfMain htf_thisModulesTests
