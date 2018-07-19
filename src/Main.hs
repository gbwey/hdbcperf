{-# OPTIONS -Wall #-}
module Main where
import qualified TestPerfOdbc as O
import qualified TestPerf as H
import qualified GenTestData as G
import Helper
import Criterion.Main

criterion :: String -> Int -> Benchmark
criterion desc sz = 
  let fn = testfn sz 
      fnWide = testfnWide sz 
  in bgroup desc [
      bench "odbc TH" $ nfIO (O.createTable "OdbcTH" >> O.testOdbcTH fn)
    , bench "odbc Raw" $ nfIO (O.createTable "OdbcRaw" >> O.testOdbcRaw fn)
    , bench "hdbc prepared" $ nfIO (H.createTable "Prepared" >> H.testPrepared fn)
--    , bench "hdbc run" $ nfIO (H.createTable "Run" >> H.testRun fn)
--    , bench "hdbc runraw" $ nfIO (H.createTable "RunRaw" >> H.testRun fn)

    , bench "odbc TH Wide" $ nfIO (O.createTableWide "OdbcTHWide" >> O.testOdbcTHWide fnWide)
    , bench "odbc Raw Wide" $ nfIO (O.createTableWide "OdbcRawWide" >> O.testOdbcRawWide fnWide)
    , bench "hdbc prepared Wide" $ nfIO (H.createTableWide "PreparedWide" >> H.testPreparedWide fnWide)

    ]

main :: IO ()
main = do
  _ <- G.createTestFileIfNotExist small
  _ <- G.createTestFileIfNotExist medium
  _ <- G.createTestFileIfNotExist large
  _ <- G.createTestFileWideIfNotExist small
  _ <- G.createTestFileWideIfNotExist medium
  _ <- G.createTestFileWideIfNotExist large
  defaultMain [
      criterion "small" small
    , criterion "medium" medium
    , criterion "large" large
    ]
  
