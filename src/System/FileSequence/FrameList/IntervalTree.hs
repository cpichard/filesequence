-- | Module FileSequence.FrameList,
-- | implement the DIET : discrete integer encoding tree
-- See reference xxxxxxx
 
module System.FileSequence.FrameList.IntervalTree where

type FrameNumber = Int
type FrameRange = (FrameNumber, FrameNumber)

-- | We use a binary tree to store the frame intervals
-- TODO : look for the function to fold this datastructure
data IntervalTree = Empty 
                  | Node FrameRange IntervalTree IntervalTree 
                  deriving Show

-- | Returns a new empty frame list 
emptyFrameList :: IntervalTree
emptyFrameList = Empty

-- | Insert a new frame
insertFrame :: IntervalTree -> FrameNumber -> IntervalTree
insertFrame Empty f = Node (f,f) Empty Empty
insertFrame node@(Node (minx, maxx) left right) f 
   | f == minx - 1 = joinRight (Node (f, maxx) left right)
   | f == maxx + 1 = joinLeft (Node (minx, f) left right)
   | f < minx - 1 = Node (minx, maxx) (insertFrame left f) right
   | f > maxx + 1 = Node (minx, maxx) left (insertFrame right f)
   | otherwise = node 
   -- We should test for this condition | otherwise =  error "trying to insert an already inserted value"

   where splitMin (Node (minx, maxx) Empty right) = (minx, maxx, right)
         splitMin (Node (minx, maxx) left right) = 
             let (u,v,l') = splitMin left in
             (u, v, Node (minx, maxx) l' right)
         splitMax (Node (minx, maxx) left Empty) = (minx, maxx, left)
         splitMax (Node (minx, maxx) left right) = 
             let (u,v,r') = splitMax right in
             (u, v, Node (minx, maxx) left r')

         joinRight node@(Node (_, _) _ Empty) = node 
         joinRight node@(Node (minx, maxx) left right) = 
            let (minx', maxx', right') = splitMin right in
            if minx'-1 == maxx 
              then Node (minx, maxx') left right'
              else node
         joinLeft node@(Node (_, _) Empty _) = node 
         joinLeft node@(Node (minx, maxx) left right) = 
            let (minx', maxx', left') = splitMax left in
            if maxx'+1 == minx 
              then Node (minx', maxx) left' right
              else node

-- | Insert a list of frames
insertFrames :: [FrameNumber] -> IntervalTree
insertFrames = foldr (flip insertFrame) emptyFrameList

-- | Test if the frame is inside the set of frames
-- use `isElementOf`
isElementOf :: FrameNumber -> IntervalTree -> Bool
isElementOf _ Empty = False
isElementOf f (Node (minx, maxx) left right)
  | f >= minx && f <= maxx = True
  | f < minx = isElementOf f left
  | f > maxx = isElementOf f right

-- | List all the frames
toList :: IntervalTree -> [FrameNumber]
toList Empty = []
toList (Node (maxx, minx) Empty Empty) = [maxx .. minx]
toList (Node (maxx, minx) left right) = toList left ++ [maxx .. minx] ++ toList right 

-- | List all the intervals
intervals :: IntervalTree -> [(FrameNumber, FrameNumber)]
intervals Empty = []
intervals (Node (maxx, minx) Empty Empty) = [(maxx, minx)]
intervals (Node (maxx, minx) left right) = intervals left ++ [(maxx, minx)] ++ intervals right 

-- | First frame 
firstFrame :: IntervalTree -> FrameNumber
firstFrame (Node (minx, _) Empty _) = minx
firstFrame (Node (_, _) left _) = firstFrame left
firstFrame Empty = 0 -- should be error ? shouldn't it ? 

-- | Last frame
lastFrame :: IntervalTree -> FrameNumber
lastFrame Empty = 0 -- should be error ? shouldn't it ?
lastFrame (Node (_, maxx) _ Empty) = maxx 
lastFrame (Node (_, _) _ right) = lastFrame right 

-- | Construct a FrameList from a range 
fromRange :: FrameNumber -> FrameNumber -> IntervalTree
fromRange ff lf | ff > lf = Node (ff,lf) Empty Empty
                | otherwise = Node (lf, ff) Empty Empty

-- | Returns the list of missing frames
-- ex :> holes [(1,5), (7,10), (15,20)]
--     > [(6,6), (11,14)]
-- FIXME : check complexity
findMin (Node (l, r) Empty nr) = l
findMin (Node (l,r) nl nr) = findMin nl
findMin Empty = error "find min in empty"
findMax (Node (l, r) nl Empty) = r
findMax (Node (l, r) nl nr) = findMax nr
findMax Empty = error "find max in empty"

holes :: IntervalTree -> IntervalTree
holes Empty = Empty
holes (Node (l, r) Empty Empty) = Empty
holes (Node (l, r) Empty nr) = Node (r+1, (findMin nr)-1) Empty (holes nr)
holes (Node (l, r) nl Empty) = Node ((findMax nl)+1, l-1) (holes nl) Empty
holes (Node (l, r) nl nr) = let rightNode = Node (r+1, (findMin nr)-1) (holes nr) Empty in Node ((findMax nl)+1, l-1) (holes nl) rightNode 

-- | Returns the number of frames
nbFrames :: IntervalTree -> FrameNumber
nbFrames Empty = 0
nbFrames fl = length $ toList fl -- TODO count nbFrames instead of creating a list of the number of frames
--nbFrames ((ff, lf):xs) = (lf-ff+1) + (nbFrames xs)

-- | Returns the number of missing frames
nbMissing :: IntervalTree -> FrameNumber
nbMissing fss = nbFrames $ holes fss

--instance Arbitrary IntervalTree where
--    arbitrary = do
--     frames_ <- listOf1 arbitrary :: Gen [FrameNumber]
--     return $ foldl insertFrame emptyFrameList frames_ 


