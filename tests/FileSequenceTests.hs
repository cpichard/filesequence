{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module FileSequenceTests  where
import System.FileSequence
import System.FileSequence.FrameList
import Data.List (sort, permutations)
import Test.Framework
import Samples

-- | Detect negative frames
test_detectNegativeFrames :: IO ()
test_detectNegativeFrames = assertEqual a b
    where b = fileSequencesFromList ["test.-0004.dpx", "test.-0003.dpx"]
          a =[FileSequence 
                { frames = fromRange (-4) (-3) 
                , padding = PaddingFixed 4 
                , path = ""
                , name = "test"
                , ext = "dpx"
                , frameSep = "."
                }] 

-- | Sequence with no name, only numbers, is valid
test_detectSequenceWithoutName :: IO ()
test_detectSequenceWithoutName = assertEqual a b 
    where b = fileSequenceFromName "0005.dpx"
          a = Just FileSequence 
                    { frames = fromRange 5 5 
                    , padding = PaddingFixed 4
                    , path = ""
                    , name = ""
                    , ext = "dpx"
                    , frameSep = ""
                    }

-- | Minus zero frame (ex: myfile.-0.tmp) is invalid
test_minusZeroIsInvalid :: IO ()
test_minusZeroIsInvalid = assertEqual a b
    where b = fileSequenceFromName "test-0.0a1"
          a = Nothing 

-- | If there is the same frame twice in a list, then the padding is
-- different and we should find two distinct sequences
test_detectSameFrameTwice :: IO ()
test_detectSameFrameTwice = do 
    assertBool $ length b == 2
    assertEqual c d 
    where b = fileSequencesFromList sameFrameTwice 
          c = isElementOf 10 $ frames (head b)
          d = isElementOf 10 $ frames (last b)

-- | Same test as above, but we test that with all permutations of the input frames 
-- we end up with the same 2 result sequences 
test_extendPaddingOrder :: IO ()
test_extendPaddingOrder = do assertBool $ all (a ==) b
  where a = fileSequencesFromList sameFrameTwice 
        b = map fileSequencesFromList (permutations sameFrameTwice)

-- |  
test_frameRestitution :: IO ()
test_frameRestitution = do assertEqual a b
    where a = sort sameFrameTwice 
          b = sort $ concatMap frameList $ fileSequencesFromList a


test_extendedPadding :: IO ()
test_extendedPadding = assertEqual a b
    where a = fileSequencesFromList extendPaddingSample
          b = [FileSequence 
                { frames = insertFrames [1, 10, 100]
                , padding = PaddingFixed 2
                , path = ""
                , name = "b"
                , ext = "tmp"
                , frameSep = "."
                }]

-- | Transparency property:
-- list of files A -> FileSequence -> list of files B
--   ==> B == A 
-- Also enforce that the list of file is not empty
prop_transparency :: FileSequence -> Bool
prop_transparency fs = sort a == sort b
  where a = frameList fs
        newfs = head $ fileSequencesFromList a
        b = frameList newfs

-- | Frames restitued by a FileSequence will produce the same FileSequence
--  fs1 --toList--> [frames] --fromList--> fs2
--   ==> fs1 == fs2
prop_bijectiveFunc :: FileSequence -> Bool
prop_bijectiveFunc fs = fs1 == fs2 --  && fs2 == fs
  where fs1 = head $ fileSequencesFromList (frameList fs)
        fs2 = head $ fileSequencesFromList (frameList fs1)

-- Detection of file sequence and insertion of frames depends on the order of the list
-- There is now a sort in the fileSequenceFromList, so the following test is not 
-- relevant anymore.
--prop_orderDoesNotMatter :: FileSequence -> Bool -- fails
--prop_orderDoesNotMatter fs = all (fs==) permuts
--  where permuts = map head $ map fileSequencesFromList $ permutations (frameList fs)

-- | Test utf8 characters
test_keepUtf8CharactersInName :: IO ()
test_keepUtf8CharactersInName = assertEqual a b
    where b = fileSequenceFromName "fffèè.0003.fg"
          a = Just FileSequence 
                    { frames = fromRange 3 3 
                    , padding = PaddingFixed 4
                    , path = ""
                    , name = "fffèè"
                    , ext = "fg"
                    , frameSep = "."
                    }

-- | Test detection of sequence with utf8 characters
test_detectSequenceFromUtf8 :: IO ()
test_detectSequenceFromUtf8 = do
    assertBool $ length b == 1
    assertEqual a (head b)
    where b = fileSequencesFromList ["ffﾩﾩΠ.001.f", "ffﾩﾩΠ.002.f", "ffﾩﾩΠ.003.f"]
          a = FileSequence
                 { frames = fromRange 1 3
                 , padding = PaddingFixed 3
                 , path = ""
                 , name = "ffﾩﾩΠ"
                 , ext = "f"
                 , frameSep = "."
                 }
