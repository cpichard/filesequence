{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Random
import Criterion.Main
import System.FileSequence
import System.FileSequence.FrameList
import Control.DeepSeq

frameListTest :: [Int] -> FrameList
--frameListTest frms = foldl addFrame [] frms
frameListTest frms = addFrames [] frms
    where addFrames sff (x:xs) = addFrames (addFrame sff x) xs
          addFrames sff [] = sff

-- Generate a list of n random frames
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
            { frames = [(-400,3000)]
            , padding = PaddingFixed 8
            , path = "/tmp/djsdk/kldls/klslds/sdiaus/mcdjdj"
            , name = "_tes_tfk__dsfjfjozeifjsjdoisodijfodsvizpcdpoczheer"
            , ext = "dpx"
            , frameSep = "."
            , extSep = "."}
  defaultMain 
    [
      bgroup "frameList" 
        [ bench "10 frames" $ whnf frameListTest [1..10]                                                  
        , bench "1000 frames" $ whnf frameListTest [1..1000]                                              
        , bench "10000 frames" $ whnf frameListTest [1..10000]                                            
        , bench "100000 frames" $ whnf frameListTest [1..100000]                                          
        , bench "10 random frames" $ whnf frameListTest frames10                                          
        , bench "1000 random frames" $ whnf frameListTest frames1000                                      
        , bench "10000 random frames" $ whnf frameListTest frames10000                                    
        -- Too long, bench "100000 random frames" $ whnf  frameListTest frames100000                       
        ],
       bgroup "string processing" 
         [ bench "fileSequenceFromList" $ whnf fileSequencesFromList listOfFiles ]
    ] 
