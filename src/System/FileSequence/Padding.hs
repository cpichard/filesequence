module System.FileSequence.Padding where

import System.FileSequence.FrameList (FrameNumber)

-- | Store Padding information 
-- We consider 2 cases:
--    * PaddingFixed:
--      when the padding is fixed and we know it because the number is prefixed with zeroes
--      like 010, 00010, etc.
--    * PaddingMax:
--      when we are not sure the padding is fixed
--      like 677 100 400
--      however it can't be superior to the number of digits
data Padding = PaddingFixed Int
             | PaddingMax Int
             deriving (Show, Eq)

-- | Paddings can be merged if they are compatible.
-- To be compatible, they should be able to restitute the frame string
--   with a printf("%0xd") 
mergePadding :: Padding -> Padding -> Maybe Padding
mergePadding (PaddingFixed a) (PaddingFixed b)
    | a == b = Just $ PaddingFixed a -- same fixed padding
    | otherwise = Nothing            -- different padding ex: 010 != 0010
mergePadding (PaddingMax a) (PaddingFixed b) 
    | b <= a = Just $ PaddingFixed b
    | otherwise = Nothing
mergePadding (PaddingFixed a) (PaddingMax b) = mergePadding (PaddingMax b) (PaddingFixed a)
mergePadding (PaddingMax a) (PaddingMax b) = Just $ PaddingMax (min a b)

-- | Deduce the padding from the frame number and the number of digit used to encode it
--   The digits does not take the minus. 
deducePadding :: FrameNumber -> Int -> Padding
deducePadding frameNb numberDigits
    | abs frameNb < 10^(numberDigits-1) = PaddingFixed numberDigits
    | otherwise = PaddingMax numberDigits



