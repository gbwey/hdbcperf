{-# LANGUAGE LambdaCase, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
module TestPerfOdbc where
import Database.ODBC.SQLServer
import qualified Database.ODBC.Internal as OI
import Database.ODBC.TH
import System.IO
import qualified Data.List.Split as SP
import Control.Monad
import Control.Exception
import TestConnectionString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String
import Data.List
import qualified Data.ByteString.Char8 as B
import Data.ByteString (ByteString)
import Helper (time)
import Control.Exception
import Data.Text (Text)
import Data.Maybe

createTable :: String -> IO ()
createTable tab = do
--  putStrLn $ "create table " ++ tab
  withConnection Auto (T.pack connstr) $ \conn -> do
    exec conn
    $ fromString
    $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
             ,"create table " ++ tab ++ " (a int, b varchar(1000), c float)"]

createTableWide :: String -> IO ()
createTableWide tab = do
--  putStrLn $ "create table " ++ tab
  withConnection Auto (T.pack connstr) $ \conn -> do
    exec conn
    $ fromString
    $ concat ["IF OBJECT_ID('dbo." ++ tab ++ "', 'U') IS NOT NULL DROP TABLE dbo." ++ tab ++ ";"
             ,"create table " ++ tab
             ," ("
             ,intercalate "," (map (\i -> "i" ++ show i ++ " int not null") [1..10::Int])
             ,","
             ,intercalate "," (map (\i -> "s" ++ show i ++ " varchar(1000) not null") [1..10::Int])
             ," )"]

createTableHex :: Text -> IO ()
createTableHex tab = do
--  putStrLn $ "create table " ++ tab
  withConnection Auto (T.pack connstr) $ \conn -> do
    OI.exec conn
    $ T.concat ["IF OBJECT_ID('dbo." <> tab <> "', 'U') IS NOT NULL DROP TABLE dbo." <> tab <> ";"
             ,"create table " <> tab <> " (b varchar(2000))"]

perf :: FilePath -> IO ()
perf fn = do
  createTable "OdbcTH"
  createTable "OdbcRaw"
  createTable "OdbcRawCommit"
  time "odbc TH" $ testOdbcTH fn
  time "odbc raw" $ testOdbcRaw fn
  time "odbc raw commit" $ testOdbcRawCommit fn


odbcHex, odbcString, odbcHexMultiple :: Text
(odbcHex, odbcString, odbcHexMultiple) = ("OdbcHex", "OdbcString", "OdbcHexMultiple")
{-
perfHex :: FilePath -> FilePath -> IO ()
perfHex fn1 fn2 = do
  createTableHex odbcHex
  createTableHex odbcString
  createTableHex odbcHexMultiple
  time odbcHex $ testOdbcHex odbcHex fn1 Nothing
  time odbcHexMultiple $ testOdbcHexMultiple odbcHexMultiple fn1 (Just 1)
  time odbcString $ testOdbcString odbcString fn2 Nothing
-}
perfWide :: FilePath -> IO ()
perfWide fn = do
  createTableWide "OdbcTHWide"
  createTableWide "OdbcRawWide"
  createTableWide "OdbcRawCommitWide"
  time "odbc TH wide" $ testOdbcTHWide fn
  time "odbc raw wide" $ testOdbcRawWide fn
  time "odbc raw wide commit" $ testOdbcRawCommitWide fn

testOdbcTH :: FilePath -> IO ()
testOdbcTH fn = do
  xs <- readFile fn
  withConnection Auto (T.pack connstr) $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      let ii :: Int = read i
      let dd :: Double = read d
      let ss :: ByteString = B.pack s
      exec conn [sql|insert into OdbcTH values ($ii,$ss,$dd)|]

commitodbc :: Connection -> Int -> Maybe Int -> IO ()
commitodbc c i = \case
  Nothing -> pure ()
  Just n -> when (i `mod` n == 0) $ commit c

testOdbcHexMultiple :: Text -> FilePath -> Maybe Int -> IO ()
testOdbcHexMultiple tab fn mcommitcnt = do
  ts <- T.readFile fn
  let commitcnt = fromMaybe 1 mcommitcnt
  withConnection Auto (T.pack connstr) $ \c -> do
    forM_ (zip [1..] $ SP.chunksOf commitcnt $ T.lines ts) $ \(i,xs) -> do
      let qq = T.intercalate "," (map (\x -> "(" <> x <> ")") xs)
      void $ queryAll' c ("insert into " <> tab <> " values " <> qq)

testOdbcHex :: Text -> FilePath -> Maybe Int -> IO ()
testOdbcHex tab fn mcommitcnt = do
  xs <- T.readFile fn
  withConnection (maybe Auto (const Manual) mcommitcnt) (T.pack connstr) $ \c -> do
    forM_ (zip [1..] $ T.lines xs) $ \(i,x) -> do
      void $ queryAll' c ("insert into " <> tab <> " values (" <> x <> ")")
      commitodbc c i mcommitcnt
--    commit c -- need this last one for left overs
{-
      lr <- try $ queryAll' c ("insert into OdbcHex values (" <> x <> ")")
      case lr of
        Left (e :: SomeException) -> do
                                       putStrLn $ "i=" ++ show i ++ " x=" ++ T.unpack x ++ " e=" ++ show e
                                       throw e
        Right [RUpd 1] -> pure ()
        o -> putStrLn $ "Expected RUpd 1 but found " ++ show o ++ " i=" ++ show i ++ " x=" ++ T.unpack x
-}

testOdbcString :: Text -> FilePath -> Maybe Int -> IO ()
testOdbcString tab fn mcommitcnt = do
  xs <- T.readFile fn
  withConnection (maybe Auto (const Manual) mcommitcnt) (T.pack connstr) $ \c -> do
    forM_ (zip [1..] $ T.lines xs) $ \(i,x) -> do
      void $ queryAll' c ("insert into " <> tab <> " values ('" <> x <> "')")
      commitodbc c i mcommitcnt
--    commit c -- need this last one for left overs
{-
      lr <- try $ queryAll' c ("insert into OdbcString values ('" <> x <> "')")
      case lr of
        Left (e :: SomeException) -> do
                                       putStrLn $ "i=" ++ show i ++ " x=" ++ T.unpack x ++ " e=" ++ show e
                                       throw e
        Right [RUpd 1] -> pure ()
        o -> putStrLn $ "Expected RUpd 1 but found " ++ show o ++ " i=" ++ show i ++ " x=" ++ T.unpack x
-}
testOdbcTHWide :: FilePath -> IO ()
testOdbcTHWide fn = do
  xs <- readFile fn
  withConnection Auto (T.pack connstr) $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,ss) -> do
      let [i1,i2,i3,i4,i5,i6,i7,i8,i9,i10::Int] = map read (take 10 ss)
      let [s1,s2,s3,s4,s5,s6,s7,s8,s9,s10] = map T.pack (drop 10 ss)
      exec conn [sql|insert into OdbcTHWide values ($i1,$i2,$i3,$i4,$i5,$i6,$i7,$i8,$i9,$i10,$s1,$s2,$s3,$s4,$s5,$s6,$s7,$s8,$s9,$s10)|]

testOdbcRaw :: FilePath -> IO ()
testOdbcRaw fn = do
  xs <- readFile fn
  withConnection Auto (T.pack connstr) $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      exec conn (fromString ("insert into OdbcRaw values(" ++ i ++ ",'" ++ s ++ "'," ++ d ++ ")"))

testOdbcRawWide :: FilePath -> IO ()
testOdbcRawWide fn = do
  xs <- readFile fn
  withConnection Auto (T.pack connstr) $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,ss) -> do
      exec conn $ fromString $ "insert into OdbcRawWide values(" ++ intercalate "," (take 10 ss) ++ ",'" ++ (intercalate "','" (drop 10 ss)) ++ "')"

testOdbcRawCommit :: FilePath -> IO ()
testOdbcRawCommit fn = do
  xs <- readFile fn
  withConnection Auto (T.pack connstr) $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,[i,s,d]) -> do
      exec conn (fromString ("insert into OdbcRawCommit values(" ++ i ++ ",'" ++ s ++ "'," ++ d ++ ")"))

testOdbcRawCommitWide :: FilePath -> IO ()
testOdbcRawCommitWide fn = do
  xs <- readFile fn
  withConnection Auto (T.pack connstr) $ \conn -> do
    forM_ (zip [1::Int ..] (SP.splitOn "\t" <$> lines xs)) $ \(n,ss) -> do
      exec conn $ fromString $ "insert into OdbcRawCommitWide values(" ++ intercalate "," (take 10 ss) ++ ",'" ++ (intercalate "','" (drop 10 ss)) ++ "')"

teststuff :: IO ()
teststuff = do
  withConnection Auto (T.pack connstr) $ \conn -> do
    let ii :: Int = 10
    let ss :: T.Text = "\"'"
    let dd :: Double =123.4
    exec conn [sql|insert into OdbcTH values ($ii,$ss,$dd)|]
