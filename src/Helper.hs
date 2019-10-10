{-# language OverloadedStrings, NumericUnderscores #-}
module Helper where
import Data.Time.Clock.POSIX
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Formatting
import Formatting.Clock
import System.Clock

time :: String -> IO a -> IO a
time txt ioa = do
  start <- getTime Monotonic
  a <- ioa
  end <- getTime Monotonic
  fprint (right 70 '_' % timeSpecs % "\n") txt start end
  return a
{-
time_ :: IO a -> IO Double
time_ act = do
  start <- getTimeH
  _ <- act
  end <- getTimeH
  return $! end - start

time :: Text -> IO a -> IO ()
time s ioa = do
 T.putStrLn $ s <> " starting ..."
 t <- time_ ioa
 T.putStrLn $ s <> ": " <> T.pack (show t)

getTimeH :: IO Double
getTimeH = realToFrac <$> getPOSIXTime
-}
testfn :: Int -> FilePath
testfn i = "test" ++ show i ++ ".dat"

testfnWide :: Int -> FilePath
testfnWide i = "testWide" ++ show i ++ ".dat"

testfnHex :: Int -> FilePath
testfnHex i = "testHex" ++ show i ++ ".dat"

testfnString :: Int -> FilePath
testfnString i = "testString" ++ show i ++ ".dat"

mega, large, medium, small :: Int
mega = 100_000
large = 10_000
medium = 1_000
small = 100

