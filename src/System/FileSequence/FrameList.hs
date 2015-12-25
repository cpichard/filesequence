-- | Module FileSequence.FrameList,
-- Implements a simple data structure to store
-- list of frames in a compact way. 
-- First approach is to store the list of contiguous frames as a tuple
-- [ (1,5) , (7,10) ] represents frames from 1 to 5 and 7 to 10
-- The first naive implementation with a list of tuple
-- seems to work just fine in terms of performances in a classical use case.
 
module System.FileSequence.FrameList where
type FrameNumber = Int
type FrameRange = (FrameNumber, FrameNumber)
type FrameList = [FrameRange]

-- | Add frame
addFrame :: FrameList -> FrameNumber -> FrameList
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
isElementOf :: FrameNumber -> FrameList -> Bool
isElementOf _ [] = False
isElementOf f ((a,b):xs)
  | f >= a && f <= b = True
  | otherwise = isElementOf f xs

-- unionFrameRange :: future programming

-- intersectFrameRange :: future programming

-- removeFrame :: future programming

-- removeFrameRange :: future programming

-- | List all the frames
toList :: FrameList -> [FrameNumber]
toList (x:xs) = [fst x .. snd x] ++ toList xs
toList [] = []

-- | First frame 
firstFrame :: FrameList -> FrameNumber
firstFrame (x:_) = fst x
firstFrame [] = 0 -- should be error ? shouldn't it ? 

-- | Last frame
lastFrame :: FrameList -> FrameNumber
lastFrame [] = 0 -- should be error ? shouldn't it ?
lastFrame x = snd (last x)

-- | Construct a FrameList from a range 
fromRange :: FrameNumber -> FrameNumber -> FrameList
fromRange ff lf = [(ff,lf)]


-- | Returns the list of missing frames
-- ex :> holes [(1,5), (7,10), (15,20)]
--     > [(6,6), (11,14)]
-- FIXME : check complexity
holes :: FrameList -> FrameList
holes [] = []
holes ((_,b):xs)
    | null xs = []
    | otherwise = (b+1, fst (head xs)-1) : holes xs

-- |Returns the number of frames
nbFrames :: FrameList -> FrameNumber
nbFrames [] = 0
nbFrames ((ff, lf):xs) = (lf-ff+1) + (nbFrames xs)

-- |Returns the number of missing frames
nbMissing :: FrameList -> FrameNumber
nbMissing fss = nbFrames $ holes fss

