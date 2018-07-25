-- stack exec hdbcperf g
-- stack exec hdbcperf r
{-# OPTIONS -Wall #-}
module TestPerf where
import qualified Database.HDBC as H
import Database.HDBC (SqlValue(..))
import qualified Database.HDBC.ODBC as H
import qualified Data.List.Split as SP
import Control.Monad
import Data.List
import Control.Exception 
import TestConnectionString (connstr)
import Helper 

--connstr :: String  
--connstr = "Driver={SQL Server Native Client 11.0};Server=?;Database=?;uid=?;pwd=?;MARS_Connection=YES"  

createTable :: String -> IO ()
createTable tab = do
--  putStrLn $ "create table " ++ tab
  runSql $ \h -> do
    H.runRaw h $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
                        ,"create table " ++ tab ++ " (a int, b varchar(1000), c float)"]
 
createTableWide :: String -> IO ()
createTableWide tab = do
--  putStrLn $ "create table " ++ tab
  runSql $ \h -> do
    H.runRaw h $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
                        ,"create table " ++ tab
                        ," (" 
                        ,intercalate "," (map (\i -> "i" ++ show i ++ " int not null") [1..10::Int])
                        ,","
                        ,intercalate "," (map (\i -> "s" ++ show i ++ " varchar(1000) not null") [1..10::Int])
                        ," )"]
 
perf :: FilePath -> IO ()  
perf fn = do
  createTable "Prepared" 
  createTable "PreparedCommit" 
  createTable "Run" 
  createTable "RunRaw" 
  time "hdbc prepared" $ testPrepared fn -- prepared 
  time "hdbc prepared commit" $ testPreparedCommit fn -- prepared 
--  time "hdbc run" $ testRun fn  -- parameter binding but prepared each time
  time "hdbc runraw" $ testRunRaw fn -- no parameter binding ie raw string

perfWide :: FilePath -> IO ()  
perfWide fn = do
  createTableWide "PreparedWide" 
  createTableWide "PreparedCommitWide" 
  createTableWide "RunWide" 
  createTableWide "RunRawWide" 
  time "hdbc prepared wide" $ testPreparedWide fn 
  time "hdbc prepared commit wide" $ testPreparedCommitWide fn 
--  time "hdbc run wide" $ testRunWide fn 
  time "hdbc runraw wide" $ testRunRawWide fn 

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
      H.commit h

testPreparedWide :: FilePath -> IO ()  
testPreparedWide fn = do
  let ins = "insert into PreparedWide values (" ++ (intercalate "," (replicate 20 "?")) ++ ")"
  xs <- readFile fn
  runSql $ \h -> do 
    bracket (H.prepare h ins) (H.finish) $ \insstmt -> do
      forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,ss) -> do
        void $ H.execute insstmt (map (SqlInteger . read) (take 10 ss) ++ map SqlString (drop 10 ss))  
  --      when (n `mod` 100 == 0) $ H.commit h
      H.commit h
  
testPreparedCommit :: FilePath -> IO ()  
testPreparedCommit fn = do
  let ins = "insert into PreparedCommit values (?,?,?)"
  xs <- readFile fn
  runSql $ \h -> do 
    bracket (H.prepare h ins) (H.finish) $ \insstmt -> do
      forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
        void $ H.execute insstmt [SqlInteger (read i), SqlString s, SqlDouble (read d)]  
        H.commit h

testPreparedCommitWide :: FilePath -> IO ()  
testPreparedCommitWide fn = do
  let ins = "insert into PreparedCommitWide values (" ++ (intercalate "," (replicate 20 "?")) ++ ")"
  xs <- readFile fn
  runSql $ \h -> do 
    bracket (H.prepare h ins) (H.finish) $ \insstmt -> do
      forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,ss) -> do
        void $ H.execute insstmt (map (SqlInteger . read) (take 10 ss) ++ map SqlString (drop 10 ss))  
        H.commit h

testRun :: FilePath -> IO ()  
testRun fn = do
  let ins = "insert into Run values (?,?,?)"
  xs <- readFile fn
  runSql $ \h -> do 
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      void $ H.run h ins [SqlInteger (read i), SqlString s, SqlDouble (read d)]
--      when (n `mod` 100 == 0) $ H.commit h
    H.commit h

testRunRaw :: FilePath -> IO ()  
testRunRaw fn = do
  xs <- readFile fn
  runSql $ \h -> do 
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      H.runRaw h ("insert into RunRaw values(" ++ i ++ ",'" ++ s ++ "'," ++ d ++ ")")
--      when (n `mod` 100 == 0) $ H.commit h
    H.commit h


testRunWide :: FilePath -> IO ()  
testRunWide fn = do
  let ins = "insert into RunWide values (?,?,?)"
  xs <- readFile fn
  runSql $ \h -> do 
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      void $ H.run h ins [SqlInteger (read i), SqlString s, SqlDouble (read d)]
--      when (n `mod` 100 == 0) $ H.commit h
    H.commit h

testRunRawWide :: FilePath -> IO ()  
testRunRawWide fn = do
  xs <- readFile fn
  runSql $ \h -> do 
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      H.runRaw h ("insert into RunRawWide values(" ++ i ++ ",'" ++ s ++ "'," ++ d ++ ")")
--      when (n `mod` 100 == 0) $ H.commit h
    H.commit h

{-
>perf largefn
create table Prepared
create table Run
create table RunRaw
hdbc prepared before
hdbc prepared: 59.21338677406311
hdbc run before
hdbc run: 158.01503801345825
hdbc runraw before
hdbc runraw: 92.51529145240784
-}