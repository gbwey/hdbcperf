{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Main where
import qualified TestPerfOdbc as O
import qualified TestPerf as H
import qualified GenTestData as G
import Helper
import Criterion.Main

criterion :: String -> Int -> Benchmark
criterion desc sz = 
    bgroup desc (let fn = testfn sz in [
      bench "odbc TH" $ nfIO (O.createTable "OdbcTH" >> O.testOdbcTH fn)
    , bench "odbc Raw" $ nfIO (O.createTable "OdbcRaw" >> O.testOdbcRaw fn)
    , bench "hdbc prepared" $ nfIO (H.createTable "Prepared" >> H.testPrepared fn)
    , bench "hdbc run" $ nfIO (H.createTable "Run" >> H.testRun fn)
    , bench "hdbc runraw" $ nfIO (H.createTable "RunRaw" >> H.testRun fn)
    ])

main :: IO ()
main = do
  _ <- G.createTestFileIfNotExist small
  _ <- G.createTestFileIfNotExist medium
  _ <- G.createTestFileIfNotExist large
  defaultMain [
      criterion "small" small
    , criterion "medium" medium
    , criterion "large" large
    ]
  
