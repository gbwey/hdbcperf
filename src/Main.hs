module Main where
import qualified TestPerfOdbc as O
import qualified TestPerf as H

main :: IO ()
main = do
  O.main
  H.main
