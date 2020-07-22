{-# LANGUAGE TypeApplications #-}

import Criterion.Main
import Criterion.Types
import GHC.DataSize
import Data.Primitive.ByteArray

listBenchmark :: Benchmark
listBenchmark = bgroup "[Int]"
    (map mkBenchmark
     [ 1000
     , 10000
     , 100000
     , 1000000
     , 10000000
     ])
  where
    mkBenchmark :: Int -> Benchmark
    mkBenchmark size = env (pure [0 .. size]) $ \lst -> bench (show size) $ nfAppIO recursiveSize lst

byteArrayBenchmark :: Benchmark
byteArrayBenchmark = bgroup "ByteArray#"
    (map mkBenchmark
     [ 1000
     , 10000
     , 100000
     , 1000000
     , 10000000
     ])
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
