{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Random
import Criterion.Main
import System.FileSequence
import System.FileSequence.FrameList
import qualified System.FileSequence.FrameList.IntervalTree as IT
import qualified System.FileSequence.FrameList.TupleList as TL
import Control.DeepSeq
import Data.List (sort)

frameListTest :: [Int] -> FrameList
--frameListTest frms = foldl insertFrame [] frms
frameListTest frms = insertFrames frms

-- | Generate a list of n random frames
randomFrames :: Int -> IO [Int]
randomFrames n = do
  gen <- getStdGen
  let mm = take n $ (randoms gen :: [Int])
  mm `deepseq` return mm 

main = do
  -- Random frames generation
  frames10 <- randomFrames 10
  frames1000 <- randomFrames 1000
  frames10000 <- randomFrames 10000
  frames100000 <- randomFrames 100000

  -- Generation of a big list of files
  -- to test string processing speed
  let listOfFiles = frameList FileSequence 
            { frames = fromRange (-400) 3000
            , padding = PaddingFixed 8
            , path = "/tmp/djsdk/kldls/klslds/sdiaus/mcdjdj"
            , name = "_tes_tfk__dsfjfjozeifjsjdoisodijfodsvizpcdpoczheer"
            , ext = "dpx"
            , frameSep = "."
            , extSep = "."}
  defaultMain 
    [
      --bgroup "insert frames" 
      --  [ bench "10 contiguous frames" $ whnf frameListTest [1..10]
      --  , bench "1000 contiguous frames" $ whnf frameListTest [1..1000]
      --  , bench "10000 contiguous frames" $ whnf frameListTest [1..10000]
      --  , bench "100000 contiguous frames" $ whnf frameListTest [1..100000]
      --  , bench "10 random frames" $ whnf frameListTest frames10
      --  , bench "1000 random frames" $ whnf frameListTest frames1000
      -- , bench "10000 random frames" $ whnf frameListTest frames10000
      --  -- , bench "100000 random frames" $ whnf  frameListTest frames100000
      --  ],
       --bgroup "process string" 
       --  [ bench "fileSequenceFromList" $ whnf fileSequencesFromList listOfFiles ],
       bgroup "insertFrames"
         [ bench "[0..10000] in interval tree" $ whnf IT.insertFrames [0..10000] 
         , bench "[0..10000] in tuple list" $ whnf TL.insertFrames [0..10000]
         , bench "10000 random in interval tree" $ whnf IT.insertFrames frames10000 
         , bench "10000 random in tuple list" $ whnf TL.insertFrames frames10000
         , bench "10000 random then sorted in tuple list" $ whnf TL.insertFrames $ sort frames10000
         , bench "100000 random then sorted in tuple list" $ whnf TL.insertFrames $ sort frames100000
         , bench "100000 random in interval tree" $ whnf IT.insertFrames $ frames100000
         ]
    ] 
