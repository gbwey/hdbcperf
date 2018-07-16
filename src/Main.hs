module Main where
import qualified TestPerfOdbc as O
import qualified TestPerf as H
import qualified GenTestData as G
import System.Environment 
import Data.List

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ('r':_):_ -> do
        O.perf G.largefn 
        H.perf G.largefn
    ('g':_):_ -> G.genTestData G.largefn 100000 
    _  -> putStrLn $ intercalate "\n" 
              ["invalid option"
              ,"g to generate large test data"
              ,"r to run"]
{-
16 17:04:40 D:\haskell\hdbcperf>stack exec hdbcperf g

16 17:05:45 D:\haskell\hdbcperf>stack exec hdbcperf r
create table OdbcTH
create table OdbcRaw
odbcTH before
odbcTH: 218.9595239162445
odbcraw before
odbcraw: 127.79130911827087
create table Prepared
create table Run
create table RunRaw
hdbc prepared before
hdbc prepared: 58.473344564437866
hdbc run before
hdbc run: 150.87962985038757
hdbc runraw before
hdbc runraw: 85.73790407180786
-}