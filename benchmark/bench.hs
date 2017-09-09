import Criterion.Main

import Data.Graph.Sort

benchdata_ttsort :: Int -> Revadlt Int ()
benchdata_ttsort 0 = []
benchdata_ttsort n = (n :<== ([],())) : benchdata_ttsort (n - 1)

bench_ttsort :: Int -> Revadlt Int ()
bench_ttsort = ttsort . benchdata_ttsort

main :: IO ()
main = defaultMain [
 bench "ttsort 100" $ nf bench_ttsort 100,
 bench "ttsort 200" $ nf bench_ttsort 200,
 bench "ttsort 300" $ nf bench_ttsort 300]
