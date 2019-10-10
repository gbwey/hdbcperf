{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified TestPerfOdbc as O
import qualified TestPerf as H
import qualified GenTestData as G
import Helper
import Criterion.Main
import Control.Monad

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

criterionHexOdbc :: String -> Int -> Maybe Int -> Benchmark
criterionHexOdbc desc rows mcommitcnt =
  let fnh = testfnHex rows
      fns = testfnString rows
      txt = " rows=" ++ show rows ++ " commit=" ++ show mcommitcnt
  in bgroup desc [
      bench ("odbc Hex" ++ txt) $ nfIO (O.createTableHex O.odbcHex >> O.testOdbcHex O.odbcHex fnh mcommitcnt)
    , bench ("odbc Hex Multiple" ++ txt) $ nfIO (O.createTableHex O.odbcHexMultiple >> O.testOdbcHexMultiple O.odbcHexMultiple fnh mcommitcnt)
    , bench ("odbc String" ++ txt) $ nfIO (O.createTableHex O.odbcString >> O.testOdbcString O.odbcString fns mcommitcnt)
    ]

criterionHexHdbc :: String -> Int -> Int -> Benchmark
criterionHexHdbc desc rows commitcnt =
  let fnh = testfnHex rows
      fns = testfnString rows
      txt = " rows=" ++ show rows ++ " commitcnt=" ++ show commitcnt
  in bgroup desc [
      bench ("hdbc ByteString prepared" ++ txt) $ nfIO (H.createTableHex "PreparedByteString" >> H.testPreparedByteString commitcnt fns)
    , bench ("hdbc ByteString Multiple prepared" ++ txt) $ nfIO (H.createTableHex "PreparedByteStringMultiple" >> H.testPreparedByteStringMultiple commitcnt fns)
    , bench ("hdbc String prepared" ++ txt) $ nfIO (H.createTableHex "PreparedString" >> H.testPreparedString commitcnt fns)
    , bench ("hdbc Hex runraw" ++ txt) $ nfIO (H.createTableHex "RunRawHex" >> H.testRunRawHex commitcnt fnh)
    , bench ("hdbc String runraw" ++ txt) $ nfIO (H.createTableHex "RunRawString" >> H.testRunRawString commitcnt fns)
    ]


main :: IO ()
main = do
  forM_ [small, medium, large] $ \i -> do
    G.createHexFiles False i
  let ws = [5,20,50,100] -- have to be divisible by small medium large else testPreparedByteStringMultiple will fail cos insert ?,?
  let ys = Nothing: map Just ws
  let zs = 1 : ws
  defaultMain (
      map (criterionHexOdbc "small" small) ys
   <> map (criterionHexHdbc "small" small) zs
   <> map (criterionHexOdbc "medium" medium) ys
   <> map (criterionHexHdbc "medium" medium) zs
   <> map (criterionHexOdbc "large" large) ys
   <> map (criterionHexHdbc "large" large) zs
   )

mainOLD :: IO ()
mainOLD = do
  forM_ [small, medium, large] $ \i -> do
    void $ G.createTestFileWideIfNotExist i
    G.createHexFiles False i
  defaultMain [
      criterion "small" small
    , criterion "medium" medium
    , criterion "large" large
    ]

do1 :: Int -> Maybe Int -> Int -> IO ()
do1 rows mcommitcnt commitcnt = do
  let fnh = testfnHex rows
      fns = testfnString rows
      txt0 s = s ++ " rows=" ++ show rows ++ " "
      txt1 s = txt0 s ++ "mcommitcnt=" ++ show mcommitcnt ++ ": "
      txt2 s = txt0 s ++ "commitcnt=" ++ show commitcnt ++ ": "

  G.createHexFiles False rows

  O.createTableHex O.odbcHex
  time (txt1 "odbc Hex") $ O.testOdbcHex O.odbcHex fnh mcommitcnt
  O.createTableHex O.odbcHexMultiple
  time (txt1 "odbc Hex Multiple") $ O.testOdbcHexMultiple O.odbcHexMultiple fnh mcommitcnt
  O.createTableHex O.odbcString
  time (txt1 "odbc String") $ O.testOdbcString O.odbcString fns mcommitcnt

  H.createTableHex "PreparedByteString"
  time (txt2 "hdbc ByteString prepared") $ H.testPreparedByteString commitcnt fns
  H.createTableHex "PreparedByteStringMultiple"
  time (txt2 "hdbc ByteString Multiple prepared") $ H.testPreparedByteStringMultiple commitcnt fns
  H.createTableHex "PreparedString"
  time (txt2 "hdbc String prepared") $ H.testPreparedString commitcnt fns
  H.createTableHex "RunRawHex"
  time (txt2 "hdbc Hex runraw") $ H.testRunRawHex commitcnt fnh
  H.createTableHex "RunRawString"
  time (txt2 "hdbc String runraw") $ H.testRunRawString commitcnt fns




