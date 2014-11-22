-- | Module FileSequence.SparseFrameList,
-- Implements a simple data structure to store
-- sparse sub-sequences of a sequence of frame
-- ex: [(1,5), (7,10)] represents frames from 1 to 5 and 7 to 10
-- The first naive implementation with a list of tuple
-- seems to work just fine in terms of performances.
 
module System.FileSequence.SparseFrameList where
type Frame = Int
type FrameRange = (Frame, Frame)
type SparseFrameList = [FrameRange]

-- type FrameRanges
--
-- | Add frame
addFrame :: SparseFrameList -> Frame -> SparseFrameList
addFrame [] f = [(f,f)]
addFrame ar@((minx, maxx):xs) f
    | f == minx-1 = (f, maxx):xs
    | f == maxx+1 = concatFrames (minx, f) xs
    | f >  maxx+1 = concatFrames (minx, maxx) (addFrame xs f)
    | f <  minx-1 = concatFrames (f,f) ar 
    | otherwise   = ar 
    where concatFrames (a,b) [] = [(a,b)]
          concatFrames (a,b) ((c,d):xss)
            | b+1 == c = (a,d):xss
            | otherwise = (a,b):((c,d):xss)

-- | Test if the frame is inside the set of frames
-- use `isElementOf`
isElementOf :: Frame -> SparseFrameList -> Bool
isElementOf _ [] = False
isElementOf f ((a,b):xs)
  | f >= a && f <= b = True
  | otherwise = isElementOf f xs

--unionFrameRange ::

--intersectFrameRange :: 

--removeFrame

-- removeFrameRange

-- | List all the frames
toList :: SparseFrameList -> [Frame]
toList (x:xs) = [fst x .. snd x] ++ toList xs
toList [] = []

-- | First frame 
firstFrame :: SparseFrameList -> Int
firstFrame (x:_) = fst x
firstFrame [] = 0 -- should be error ? shouldn't it ? 

-- | Last frame
lastFrame :: SparseFrameList -> Int
lastFrame [] = 0 -- should be error ? shouldn't it ?
lastFrame x = snd (last x)

-- | Construct a SparseFrameList from a range 
fromRange :: Int -> Int -> SparseFrameList
fromRange ff lf = [(ff,lf)]


-- | Returns the list of missing frames
-- ex : holes [(1,5), (7,10), (15,20)]
--      [(6,6), (11,14)]
-- FIXME : check complexity
holes :: SparseFrameList -> SparseFrameList
holes [] = []
holes ((_,b):xs)
    | null xs = []
    | otherwise = (b+1, fst (head xs)-1) : holes xs
