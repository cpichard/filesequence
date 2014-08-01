module Main where
import Control.DeepSeq
import Criterion.Main
import System.FileSequence.SparseFrameList
import System.Random

--addFrame :: SparseFrameList -> Frame -> SparseFrameList
sparseFrameListTest :: [Int] -> SparseFrameList
sparseFrameListTest frms = foldl addFrame [] frms

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

  defaultMain 
    [
      bgroup "sparseFrameList" 
        [ bench "10 frames" $ whnf  sparseFrameListTest [1..10]                                                  
        , bench "1000 frames" $ whnf  sparseFrameListTest [1..1000]                                              
        , bench "10000 frames" $ whnf  sparseFrameListTest [1..10000]                                            
        , bench "100000 frames" $ whnf  sparseFrameListTest [1..100000]                                          
        , bench "10 random frames" $ whnf  sparseFrameListTest frames10                                          
        , bench "1000 random frames" $ whnf  sparseFrameListTest frames1000                                      
        , bench "10000 random frames" $ whnf  sparseFrameListTest frames10000                                    
        -- Too long, bench "100000 random frames" $ whnf  sparseFrameListTest frames100000                       
        ]
    ] 