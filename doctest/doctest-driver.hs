-- {-# OPTIONS_GHC -F -pgmF doctest-discover -optF doctest-config.json #-}

import Test.DocTest

main :: IO ()
main = doctest ["-i/src"]
