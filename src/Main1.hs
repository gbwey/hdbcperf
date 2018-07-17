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
      flag' mega
       ( long "mega"
      <> short 'x'
      <> help ("mega file size " ++ show mega) )
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
