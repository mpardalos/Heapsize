{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Criterion.Types
import GHC.DataSize
import Data.Primitive.ByteArray

listBenchmark :: Benchmark
listBenchmark = bgroup "[Int]" $
  map mkBenchmark
    [ 1000
    , 100000
    , 10000000
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

main :: IO ()
main =
  defaultMainWith
    (defaultConfig {reportFile = Just "ghc-datasize-benchmark.html"})
    [ listBenchmark, byteArrayBenchmark ]
