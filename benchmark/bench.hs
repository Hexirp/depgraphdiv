{-# OPTIONS_GHC -fno-warn-orphans #-}

import Criterion.Main
import Control.DeepSeq

import Data.Graph.Sort

instance (NFData v, NFData t) => NFData (Revadlet v t) where
 rnf (v :<== rt) = deepseq v (deepseq rt ())

benchdata_ttsort :: Int -> Revadlt Int ()
benchdata_ttsort = map el . flip take [0..] where
 el n = n :<== (take n [0..], ())

bench_ttsort :: Int -> Revadlt Int ()
bench_ttsort = ttsort . benchdata_ttsort

main :: IO ()
main = defaultMain [
 bgroup "ttsort" [
  bench "100" $ nf bench_ttsort 100,
  bench "200" $ nf bench_ttsort 200,
  bench "300" $ nf bench_ttsort 300,
  bench "400" $ nf bench_ttsort 400]]
