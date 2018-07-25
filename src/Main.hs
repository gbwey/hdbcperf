{-# OPTIONS -Wall #-}
module Main where
import qualified TestPerfOdbc as O
import qualified TestPerf as H
import qualified GenTestData as G
import Helper
import Criterion.Main

criterion :: String -> Int -> Benchmark
criterion desc sz = 
  bgroup desc (
     (let fn = testfn sz in [
      bench "odbc TH" $ nfIO (O.createTable "OdbcTH" >> O.testOdbcTH fn)
    , bench "odbc Raw" $ nfIO (O.createTable "OdbcRaw" >> O.testOdbcRaw fn)
    , bench "odbc Raw Commit" $ nfIO (O.createTable "OdbcRawCommit" >> O.testOdbcRawCommit fn)
    , bench "hdbc prepared" $ nfIO (H.createTable "Prepared" >> H.testPrepared fn)
    , bench "hdbc prepared commit" $ nfIO (H.createTable "PreparedCommit" >> H.testPreparedCommit fn)
--    , bench "hdbc run" $ nfIO (H.createTable "Run" >> H.testRun fn)
--    , bench "hdbc runraw" $ nfIO (H.createTable "RunRaw" >> H.testRunRaw fn)
    ]) ++
     (let fn = testfnWide sz in [
      bench "odbc TH Wide" $ nfIO (O.createTableWide "OdbcTHWide" >> O.testOdbcTHWide fn)
    , bench "odbc Raw Wide" $ nfIO (O.createTableWide "OdbcRawWide" >> O.testOdbcRawWide fn)
    , bench "odbc Raw Commit Wide" $ nfIO (O.createTableWide "OdbcRawCommitWide" >> O.testOdbcRawCommitWide fn)
    , bench "hdbc prepared Wide" $ nfIO (H.createTableWide "PreparedWide" >> H.testPreparedWide fn)
    , bench "hdbc prepared commit Wide" $ nfIO (H.createTableWide "PreparedCommitWide" >> H.testPreparedCommitWide fn)
--    , bench "hdbc run wide" $ nfIO (H.createTable "RunWide" >> H.testRunWide fn)
--    , bench "hdbc runraw wide" $ nfIO (H.createTable "RunRawWide" >> H.testRunRawWide fn)
    ]))

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
  
