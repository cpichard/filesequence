module System.FileSequence.FrameList.TupleList where
-- Store a list of intervals as a list of tuple
-- API compatible with FrameList
type FrameNumber = Int
type FrameRange = (FrameNumber, FrameNumber)
type TupleList = [FrameRange]

-- | Returns an empty tuple list
emptyFrameList :: TupleList
emptyFrameList = []

-- | TupleList stores intervals, so returns the structure
intervals :: TupleList -> [(FrameNumber, FrameNumber)]
intervals k = k

-- | Insert a frame
insertFrame :: TupleList -> FrameNumber -> TupleList
insertFrame [] f = [(f,f)]
insertFrame ar@((minx, maxx):xs) f
    | f == minx-1 = (f, maxx):xs
    | f == maxx+1 = concatFrames (minx, f) xs
    | f >  maxx+1 = concatFrames (minx, maxx) (insertFrame xs f)
    | f <  minx-1 = concatFrames (f,f) ar 
    | otherwise   = ar 
    where concatFrames (a,b) [] = [(a,b)]
          concatFrames (a,b) ((c,d):xss)
            | b+1 == c = (a,d):xss
            | otherwise = (a,b):((c,d):xss)

-- | Insert a list of frames
insertFrames :: [FrameNumber] -> TupleList
insertFrames = foldr (flip insertFrame) emptyFrameList

-- | Test if the frame is inside the set of frames
-- use `isElementOf`
isElementOf :: FrameNumber -> TupleList -> Bool
isElementOf _ [] = False
isElementOf f ((a,b):xs)
  | f >= a && f <= b = True
  | otherwise = isElementOf f xs

-- | List all the frames
toList :: TupleList -> [FrameNumber]
toList (x:xs) = [fst x .. snd x] ++ toList xs
toList [] = []

-- | First frame 
firstFrame :: TupleList -> FrameNumber
firstFrame (x:_) = fst x
firstFrame [] = 0 -- should be error ? shouldn't it ? 

-- | Last frame
lastFrame :: TupleList -> FrameNumber
lastFrame [] = 0 -- should be error ? shouldn't it ?
lastFrame x = snd (last x)

-- | Construct a TupleList from a range 
fromRange :: FrameNumber -> FrameNumber -> TupleList
fromRange ff lf = [(ff,lf)]

-- | Returns the list of missing frames
-- ex :> holes [(1,5), (7,10), (15,20)]
--     > [(6,6), (11,14)]
-- FIXME : check complexity
holes :: TupleList -> TupleList
holes [] = []
holes ((_,b):xs)
    | null xs = []
    | otherwise = (b+1, fst (head xs)-1) : holes xs

-- | Returns the number of frames
nbFrames :: TupleList -> FrameNumber
nbFrames [] = 0
nbFrames ((ff, lf):xs) = (lf-ff+1) + (nbFrames xs)

-- | Returns the number of missing frames
nbMissing :: TupleList -> FrameNumber
nbMissing fss = nbFrames $ holes fss


