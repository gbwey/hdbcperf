module Helper where
import Data.Time.Clock.POSIX

time_ :: IO a -> IO Double
time_ act = do
  start <- getTime
  _ <- act
  end <- getTime
  return $! end - start

time :: String -> IO a -> IO ()
time s ioa = do 
 putStrLn $ s ++ " starting ..."
 t <- time_ ioa 
 putStrLn $ s ++ ": " ++ show t

getTime :: IO Double
getTime = realToFrac <$> getPOSIXTime

testfn :: Int -> FilePath
testfn i = "test" ++ show i ++ ".dat"

