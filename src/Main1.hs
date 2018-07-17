{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
module Main1 where
import qualified TestPerfOdbc as O
import qualified TestPerf as H
import qualified GenTestData as G
import Options.Applicative hiding (option,header)
import Options.Applicative 
import Helper
import Data.Monoid
  
main :: IO ()
main = do
  runit =<< execParser opts
 where
    opts = info (myOptions <**> helper)
      ( fullDesc
     <> progDesc "hdbc odbc performance tests"
     <> header "hdbc odbc performance tests")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

runit :: MyOptions -> IO ()
runit MyOptions {..} = do
  case oCmd of
    Gen -> G.genTestData oRows 
    Run -> do
      fn <- G.createTestFileIfNotExist oRows
      O.perf fn
      H.perf fn

data MyOptions = MyOptions { oRows :: !Int, oCmd :: Cmd } deriving Show
data Cmd = Gen | Run deriving Show

myOptions :: Parser MyOptions
myOptions = MyOptions 
   <$>       
      (flag' large
        ( long "large"
       <> short 'l'
       <> help ("large file size " ++ show large) )
     <|> 
      flag' medium
       ( long "medium"
      <> short 'm'
      <> help ("medium file size " ++ show medium) )
     <|> 
      flag' small
       ( long "small"
      <> short 's'
      <> help ("small file size " ++ show small) )
     <|> 
      option auto 
       ( long "rows"
      <> short 'i'
      <> metavar "INT"
      <> help "specify number of rows" ))
   <*> subparser (
          command "gen"  (pure Gen `withInfo` "generate test data for given row size")
       <> command "run" (pure Run `withInfo` "run simple tests")
                 )

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


-- running with hackage version so no real diff
16 18:20:01 d:\haskell\hdbcperf>stack exec hdbcperf r
create table OdbcTH
create table OdbcRaw
odbc TH starting ...
odbc TH: 142.04712462425232
odbc raw starting ...
odbc raw: 123.65307259559631
create table Prepared
create table Run
create table RunRaw
hdbc prepared starting ...
hdbc prepared: 120.46089005470276
hdbc run starting ...
hdbc run: 150.8586287498474
hdbc runraw starting ...

-- running with hackage version so no real diff with prep cos messed up
16 18:30:00 d:\haskell\hdbcperf>stack exec hdbcperf r
create table OdbcTH
create table OdbcRaw
odbc TH starting ...
odbc TH: 151.9226894378662
odbc raw starting ...
odbc raw: 128.90837335586548
create table Prepared
create table Run
create table RunRaw
hdbc prepared starting ...
hdbc prepared: 116.85568380355835
hdbc run starting ...
hdbc run: 148.7855100631714
hdbc runraw starting ...
hdbc runraw: 80.94062948226929

16 18:40:46 d:\haskell\hdbcperf>stack exec hdbcperf r
create table OdbcTH
create table OdbcRaw
odbc TH starting ...
odbc TH: 148.08446979522705
odbc raw starting ...
odbc raw: 130.31345343589783
create table Prepared
create table Run
create table RunRaw
hdbc prepared starting ...
hdbc prepared: 59.89642572402954
hdbc run starting ...
hdbc run: 149.38754439353943
hdbc runraw starting ...
hdbc runraw: 81.44465827941895

-- uses my github/gbwey account for hdbc and hdbc-odbc
16 19:51:42 d:\haskell\hdbcperf>stack exec hdbcperf r
create table OdbcTH
create table OdbcRaw
odbc TH starting ...
odbc TH: 147.73144960403442
odbc raw starting ...
odbc raw: 131.99755001068115
create table Prepared
create table Run
create table RunRaw
hdbc prepared starting ...
hdbc prepared: 56.62623882293701
hdbc run starting ...
hdbc run: 149.138530254364
hdbc runraw starting ...
hdbc runraw: 82.74873280525208
-}