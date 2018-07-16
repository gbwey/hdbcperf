{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module TestPerfOdbc where
import Database.ODBC.SQLServer
import Database.ODBC.TH
import System.IO
import qualified Data.List.Split as SP
import Control.Monad
import Data.Time.Clock.POSIX
import Control.Exception 
import TestConnectionString (connstr)
import qualified Data.Text as T
import Data.String
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)

main :: IO ()
main = perf largefn

smallfn, largefn :: FilePath
smallfn = "testsmall.dat" -- 1000
largefn = "testlarge.dat" -- 100,000

createTable :: String -> IO ()
createTable tab = do
  putStrLn $ "create table " ++ tab
  bracket (connect $ T.pack connstr) close $ \conn -> do
    exec conn 
    $ fromString 
    $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
             ,"create table " ++ tab ++ " (a int, b varchar(1000), c float)"]
 
perf :: FilePath -> IO ()  
perf fn = do
  createTable "OdbcTH" 
  createTable "OdbcRaw" 
  time "odbcTH" $ testOdbcTH fn  
  time "odbcraw" $ testOdbcRaw fn 
  
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

testOdbcTH :: FilePath -> IO ()  
testOdbcTH fn = do
  xs <- readFile fn
  bracket (connect $ T.pack connstr) close $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      let ii :: Int = read i
      let dd :: Double = read d
      let ss :: ByteString = B.pack s
      exec conn [sql|insert into OdbcTH values ($ii,$ss,$dd)|]
    --  when (n `mod` 100 == 0) $ commit conn

testOdbcRaw :: FilePath -> IO ()  
testOdbcRaw fn = do
  xs <- readFile fn
  bracket (connect $ T.pack connstr) close $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      exec conn (fromString ("insert into OdbcRaw values(" ++ i ++ ",'" ++ s ++ "'," ++ d ++ ")"))
    --  when (n `mod` 100 == 0) $ commit conn
{-
*TestPerfOdbc> main
create table OdbcTH
create table OdbcRaw
odbcTH before
odbcTH: 155.40788865089417
odbcraw before
odbcraw: 139.72199153900146
-}