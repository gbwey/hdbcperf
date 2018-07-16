{-# OPTIONS -Wall #-}
module TestPerf where
import Test.QuickCheck
import System.IO
import qualified Database.HDBC as H
import Database.HDBC (SqlValue(..))
import qualified Database.HDBC.ODBC as H
import qualified Data.List.Split as SP
import Control.Monad
import Data.Time.Clock.POSIX
import Control.Exception 
import TestConnectionString (connstr)

--connstr :: String  
--connstr = "Driver={SQL Server Native Client 11.0};Server=?;Database=?;uid=?;pwd=?;MARS_Connection=YES"  
main :: IO ()
main = perf largefn

dolarge :: IO ()
dolarge = do
  genTestData largefn 100000
  perf largefn

smallfn, largefn :: FilePath
smallfn = "testsmall.dat" -- 1000
largefn = "testlarge.dat" -- 100,000

genTestData :: FilePath -> Int -> IO ()
genTestData fn nm = do
 let g1 = do
            i <- choose (-100000::Int,100000)
            n <- choose (1,1000) 
            s <- vectorOf n (oneof [choose ('a','z'), choose ('0','9')])
            d <- choose (-100000.0::Double,100000.0)
            return (i,s,d)
 withFile fn WriteMode $ \h -> do
   replicateM_ nm $ do
     (i,s,d) <- generate g1
     hPutStrLn h $ show i ++ "\t" ++ s ++ "\t" ++ show d

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
  time "prepared" $ testPrepared fn -- prepared 
  time "run" $ testRun fn  -- parameter binding but prepared each time
  time "runraw" $ testRunRaw fn -- no parameter binding ie raw string

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
        when (n `mod` 100 == 0) $ H.commit h
  
time_ :: IO a -> IO Double
time_ act = do
  start <- getTime
  _ <- act
  end <- getTime
  return $! end - start

time :: String -> IO a -> IO ()
time s ioa = do 
 putStrLn $ s ++ " before"
 t <- time_ ioa 
 putStrLn $ s ++ ": " ++ show t

getTime :: IO Double
getTime = realToFrac <$> getPOSIXTime

testRun :: FilePath -> IO ()  
testRun fn = do
  let ins = "insert into Run values (?,?,?)"
  xs <- readFile fn
  runSql $ \h -> do 
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      void $ H.run h ins [SqlInteger (read i), SqlString s, SqlDouble (read d)]
      when (n `mod` 100 == 0) $ H.commit h

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