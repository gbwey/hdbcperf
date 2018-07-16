-- stack exec hdbcperf g
-- stack exec hdbcperf r
{-# OPTIONS -Wall #-}
module TestPerf where
import qualified Database.HDBC as H
import Database.HDBC (SqlValue(..))
import qualified Database.HDBC.ODBC as H
import qualified Data.List.Split as SP
import Control.Monad
import Control.Exception 
import TestConnectionString (connstr)
import Helper (time)
--connstr :: String  
--connstr = "Driver={SQL Server Native Client 11.0};Server=?;Database=?;uid=?;pwd=?;MARS_Connection=YES"  

createTable :: String -> IO ()
createTable tab = do
  putStrLn $ "create table " ++ tab
  runSql $ \h -> do
    H.runRaw h $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
                        ,"create table " ++ tab ++ " (a int, b varchar(1000), c float)"]
 
perf :: FilePath -> IO ()  
perf fn = do
  createTable "Prepared" 
  createTable "Run" 
  createTable "RunRaw" 
  time "hdbc prepared" $ testPrepared fn -- prepared 
  time "hdbc run" $ testRun fn  -- parameter binding but prepared each time
  time "hdbc runraw" $ testRunRaw fn -- no parameter binding ie raw string

runSql :: (H.Connection -> IO c) -> IO c
runSql = bracket (H.connectODBC connstr) H.disconnect . flip H.withTransaction 

testPrepared :: FilePath -> IO ()  
testPrepared fn = do
  let ins = "insert into Prepared values (?,?,?)"
  xs <- readFile fn
  runSql $ \h -> do 
    bracket (H.prepare h ins) (H.finish) $ \insstmt -> do
      forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
        void $ H.execute insstmt [SqlInteger (read i), SqlString s, SqlDouble (read d)]  
  --      when (n `mod` 100 == 0) $ H.commit h
  
testRun :: FilePath -> IO ()  
testRun fn = do
  let ins = "insert into Run values (?,?,?)"
  xs <- readFile fn
  runSql $ \h -> do 
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      void $ H.run h ins [SqlInteger (read i), SqlString s, SqlDouble (read d)]
  --    when (n `mod` 100 == 0) $ H.commit h

testRunRaw :: FilePath -> IO ()  
testRunRaw fn = do
  xs <- readFile fn
  runSql $ \h -> do 
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      H.runRaw h ("insert into RunRaw values(" ++ i ++ ",'" ++ s ++ "'," ++ d ++ ")")
      when (n `mod` 100 == 0) $ H.commit h

{-
>perf largefn
create table Prepared
create table Run
create table RunRaw
prepared before
prepared: 59.21338677406311
run before
run: 158.01503801345825
runraw before
runraw: 92.51529145240784
-}