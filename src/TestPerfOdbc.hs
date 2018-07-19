{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module TestPerfOdbc where
import Database.ODBC.SQLServer
import Database.ODBC.TH
import System.IO
import qualified Data.List.Split as SP
import Control.Monad
import Control.Exception 
import TestConnectionString (connstr)
import qualified Data.Text as T
import Data.String
import Data.List
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Helper (time)

createTable :: String -> IO ()
createTable tab = do
--  putStrLn $ "create table " ++ tab
  bracket (connect $ T.pack connstr) close $ \conn -> do
    exec conn 
    $ fromString 
    $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
             ,"create table " ++ tab ++ " (a int, b varchar(1000), c float)"]
 
createTableWide :: String -> IO ()
createTableWide tab = do
--  putStrLn $ "create table " ++ tab
  bracket (connect $ T.pack connstr) close $ \conn -> do
    exec conn 
    $ fromString 
    $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
             ,"create table " ++ tab
             ," (" 
             ,intercalate "," (map (\i -> "i" ++ show i ++ " int not null") [1..10::Int])
             ,","
             ,intercalate "," (map (\i -> "s" ++ show i ++ " varchar(1000) not null") [1..10::Int])
             ," )"] 
 
perf :: FilePath -> IO ()  
perf fn = do
  createTable "OdbcTH" 
  createTable "OdbcRaw" 
  time "odbc TH" $ testOdbcTH fn  
  time "odbc raw" $ testOdbcRaw fn 
  
perfWide :: FilePath -> IO ()  
perfWide fn = do
  createTableWide "OdbcTHWide" 
  createTableWide "OdbcRawWide" 
  time "odbc TH wide" $ testOdbcTHWide fn 
  time "odbc raw wide" $ testOdbcRawWide fn 
  
testOdbcTH :: FilePath -> IO ()  
testOdbcTH fn = do
  xs <- readFile fn
  bracket (connect $ T.pack connstr) close $ \conn -> do
    exec conn "set implicit_transactions on"
    exec conn "begin tran"
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      let ii :: Int = read i
      let dd :: Double = read d
      let ss :: ByteString = B.pack s
      exec conn [sql|insert into OdbcTH values ($ii,$ss,$dd)|]
    exec conn "set implicit_transactions off"
    exec conn "commit"

testOdbcTHWide :: FilePath -> IO ()  
testOdbcTHWide fn = do
  xs <- readFile fn
  bracket (connect $ T.pack connstr) close $ \conn -> do
    exec conn "set implicit_transactions on"
    exec conn "begin tran"
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,ss) -> do
      let [i1,i2,i3,i4,i5,i6,i7,i8,i9,i10::Int] = map read (take 10 ss)
      let [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10] = map T.pack (drop 10 ss)
      exec conn [sql|insert into OdbcTHWide values ($i1,$i2,$i3,$i4,$i5,$i6,$i7,$i8,$i9,$i10,$s1,$s2,$s3,$s4,$s5,$s6,$s7,$s8,$s9,$s10)|]
    exec conn "set implicit_transactions off"
    exec conn "commit"

testOdbcRaw :: FilePath -> IO ()  
testOdbcRaw fn = do
  xs <- readFile fn
  bracket (connect (T.pack connstr)) close $ \conn -> do
    exec conn "set implicit_transactions on"
    exec conn "begin tran"
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      exec conn (fromString ("insert into OdbcRaw values(" ++ i ++ ",'" ++ s ++ "'," ++ d ++ ")"))
    exec conn "set implicit_transactions off"
    exec conn "commit"

testOdbcRawWide :: FilePath -> IO ()  
testOdbcRawWide fn = do
  xs <- readFile fn
  bracket (connect (T.pack connstr)) close $ \conn -> do
    exec conn "set implicit_transactions on"
    exec conn "begin tran"
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,ss) -> do
      exec conn $ fromString $ "insert into OdbcRawWide values(" ++ intercalate "," (take 10 ss) ++ ",'" ++ (intercalate "','" (drop 10 ss)) ++ "')"
    exec conn "set implicit_transactions off"
    exec conn "commit"

