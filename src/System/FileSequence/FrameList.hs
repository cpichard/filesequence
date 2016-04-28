-- | Module FileSequence.FrameList,
--   Functions to store a list of frames efficiently

module System.FileSequence.FrameList where

import Test.QuickCheck

-- Import the chosen implementation: IntervalTree or TupleList 
-- IntervalTree is still WIP and not passing the tests yet

import qualified System.FileSequence.FrameList.IntervalTree as Impl
newtype FrameList = FL Impl.IntervalTree
-- or :
--import qualified System.FileSequence.FrameList.TupleList as Impl
--newtype FrameList = FL Impl.TupleList 
-- Note that newtype is used so the Arbitrary instance is not a Synonym

type FrameNumber = Impl.FrameNumber
type FrameRange = Impl.FrameRange 

instance Eq FrameList where
  (==) (FL a) (FL b) = Impl.intervals a == Impl.intervals b  

instance Show FrameList where
  show (FL a) = show a

-- | Returns an empty frame list
emptyFrameList :: FrameList
emptyFrameList = FL Impl.emptyFrameList

-- | Insert one frame in the list
-- | the FrameList structure works like a set so if 
--   the frame already exists it is not inserted again
insertFrame :: FrameList -> FrameNumber -> FrameList
insertFrame (FL a) fn = FL (Impl.insertFrame a fn)

-- | Insert a list of frames 
insertFrames :: [FrameNumber] -> FrameList
insertFrames fl = FL (Impl.insertFrames fl) 

-- | Test if a given frame is in the list
isElementOf :: FrameNumber -> FrameList -> Bool
isElementOf fn (FL a) = Impl.isElementOf fn a

-- | Returns a list of frame numbers
toList :: FrameList -> [FrameNumber]
toList (FL a)= Impl.toList a

-- | Returns a list of intervals
intervals :: FrameList -> [(FrameNumber, FrameNumber)]
intervals (FL a) = Impl.intervals a

-- | Returns the first frame
firstFrame :: FrameList -> FrameNumber
firstFrame (FL a) = Impl.firstFrame a

-- | Returns the last frame
lastFrame :: FrameList -> FrameNumber
lastFrame (FL a) = Impl.lastFrame a

-- | Create a FrameList from an inclusive range 
fromRange :: FrameNumber -> FrameNumber -> FrameList
fromRange a b = FL (Impl.fromRange a b)

-- | Returns the missing frames as a FrameList
holes :: FrameList -> FrameList
holes (FL h) = FL (Impl.holes h)

-- | Returns the number of frames stored in the list
nbFrames :: FrameList -> FrameNumber
nbFrames (FL h) = Impl.nbFrames h

-- | Returns the number of missing frames in the list
nbMissing :: FrameList -> FrameNumber
nbMissing (FL h) = Impl.nbMissing h

-- | Generate random arbitrary FrameList
instance Arbitrary FrameList where
    arbitrary = do
     frames_ <- listOf1 arbitrary :: Gen [FrameNumber]
     return $ foldl insertFrame emptyFrameList frames_

-- Future programming:
-- unionFrameList :: 
-- intersectFrameList :: 
-- removeFrame :: 
-- removeFrameRange :: 
--
