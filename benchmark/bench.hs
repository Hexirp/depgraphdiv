{-# OPTIONS_GHC -fno-warn-orphans #-}

import Prelude
import Criterion.Main
import Control.DeepSeq

import Data.Graph.Sort(Revadlet(..), Revadlt, ttsort)

instance (NFData v, NFData t) => NFData (Revadlet v t) where
 rnf (v :<== rt) = deepseq v (deepseq rt ())

benchdata_full :: Int -> Revadlt Int ()
benchdata_full m = map el $ take m [0..] where
 el n = n :<== (take n [0..], ())

bench_full :: Int -> Revadlt Int ()
bench_full = ttsort . benchdata_full

benchnfi :: NFData a => (Int -> a) -> Int -> Benchmark
benchnfi f n = bench (show n) $ nf f n

main :: IO ()
main = defaultMain [
 bgroup "full" [
  benchnfi bench_full 100,
  benchnfi bench_full 200,
  benchnfi bench_full 300,
  benchnfi bench_full 400]]
