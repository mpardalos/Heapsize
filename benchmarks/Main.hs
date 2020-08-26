{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Criterion.Main
import Criterion.Types
import HeapSize
import GHC.Generics (Generic)
import Data.Primitive.ByteArray
import Data.IORef
import Control.DeepSeq

listBenchmark :: Benchmark
listBenchmark = bgroup "[Int]" $
  map mkBenchmark
    [ 1000
    , 10000
    , 100000
    , 1000000
    ]
  where
    mkBenchmark :: Int -> Benchmark
    mkBenchmark size = env (pure [0 .. size]) $ \lst -> bench (show size) $ nfAppIO recursiveSize lst

byteArrayBenchmark :: Benchmark
byteArrayBenchmark = bgroup "ByteArray#" $
  -- Just to make sure that it's O(1)
  map mkBenchmark [10, 10000]
  where
    mkBenchmark :: Int -> Benchmark
    mkBenchmark size = env
      (newByteArray size)
      (\lst -> bench (show size) $ nfAppIO recursiveSize lst)


data C = C Int (IORef C)
  deriving (Generic, NFData)

circularBenchmark :: Benchmark
circularBenchmark = env mkT (\c -> bench "Circular" (nfAppIO recursiveSize c))
  where
    mkT :: IO C
    mkT = do
      rec first <- newIORef (C 1 second)
          second <- newIORef (C 2 third)
          third <- newIORef (C 3 first)
      readIORef first


main :: IO ()
main =
  defaultMainWith
    (defaultConfig {reportFile = Just "ghc-datasize-benchmark.html"})
    [ listBenchmark, byteArrayBenchmark, circularBenchmark ]
