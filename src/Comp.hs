{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
module Comp where
import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Function
import Control.Monad

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

fn1, fn2, fn3, fn4, fn5, fn6, fn7 :: FilePath
fn1 = "ubuntu1604_virtualbox1_20170724.txt"
fn2 = "ubuntu1604_virtualbox2_20170724.txt"
fn3 = "ubuntu1604_virtualbox_shuffle1_20170725.txt"
fn4 = "windows1_20170724.txt"
fn5 = "ubuntu1604_virtualbox1_HACKAGEHDBC_20170725.txt"
fn6 = "ubuntu1604_virtualbox3_20170726.txt"
fn7 = "windows_hackagehdbc.txt"

load :: FilePath -> IO (Map String FS)
load fn = do
  xs <- lines <$> readFile fn 
  return $ M.fromListWith (\a b -> error $ "duplicate " ++ show (a,b)) (loadImpl xs)

data FS = FS Float Measure deriving (Show,Eq)
data Measure = Sec | Milli deriving (Show,Eq)

getMeasure :: FS -> Measure
getMeasure (FS _ x) = x

loadImpl :: [String] -> [(String, FS)]
loadImpl ss =
  let zs = concat $ ss <&> 
             \s -> case words s of
                     "benchmarking":ys -> [Left (unwords ys)]
                     "mean":a:b:_ -> case b of 
                                      "s" -> [Right $ FS (read a) Sec]
                                      "ms" -> [Right $ FS (read a) Milli]
                                      _ -> error $ "unknown mean measure " ++ b
                     _ -> []
     
      ws = take ((length zs `div` 2) * 2) zs
   in chunksOf 2 ws <&> \case 
              [Left a,Right b] -> (a,b)
              o -> error $ "oops expected left right pair only!" ++ show o
                                                 
chunksOf :: Int -> [a] -> [[a]]                   
chunksOf n = unfoldr (\s -> if null s then Nothing else Just $ splitAt n s) 

comp :: FilePath -> FilePath -> IO (Map String (FS,FS))
comp f1 f2 = do
  m1 <- load f1
  m2 <- load f2
  forM_ (M.keys (m1 M.\\ m2)) $ \k -> putStrLn $ ">>> warning: " ++ padRight 40 k ++ " key missing from right" 
  forM_ (M.keys (m2 M.\\ m1)) $ \k -> putStrLn $ ">>> warning: " ++ padRight 40 k ++ " key missing from left"
  return $ M.intersectionWith (,) m1 m2

goodbad :: Float -> Map String (FS, FS) -> (Map String (FS, FS), Map String (FS, FS))
goodbad e = M.partition (\(a,b) -> let v = on (/) calc a b in v >= 1-e && v <= 1+e)

calc :: FS -> Float
calc (FS f x) = f * case x of 
                 Sec -> 1000
                 Milli -> 1

dump :: FilePath -> FilePath -> IO () 
dump = dump' 0.1

dump' :: Float -> FilePath -> FilePath -> IO () 
dump' e f1 f2 = do
  m <- comp f1 f2 
  let (a,b) = goodbad e m
  prt ("good:within tolerance " ++ show e) a
  prt "bad" b
  
prt :: String -> Map String (FS, FS) -> IO ()
prt s m | null m = return ()
        | otherwise = do
                        putStrLn $ "\n" ++ s ++ " count=" ++ show (length m)
                        forM_ (M.toList m) $ \(k,fs2) -> putStrLn $ padRight 40 k ++ " " ++ prt' fs2

prt' :: (FS,FS) -> String
prt' z@(f1, f2) = 
   let xx = if on (==) getMeasure f1 f2 then "" else "Different measures!"
   in padLeft 10 (prt'' f1) ++ " " ++ padLeft 10 (prt'' f2) ++ padLeft 10 (show (ratio z)) ++ " " ++ xx

ratio :: (FS,FS) -> Float 
ratio (f1,f2) = 
  let a = calc f1
      b = calc f2
  in fromIntegral (truncate ((if a > b then a / b else - b / a) * 100)) / 100

prt'' :: FS -> String
prt'' (FS f x) = show f ++ case x of 
                             Sec -> "s"
                             Milli -> "ms"

padRight :: Int -> String -> String
padRight n s | length s > n = s
        | otherwise = take n (s ++ repeat ' ')

padLeft :: Int -> String -> String
padLeft n s | length s > n = s
            | otherwise = replicate (n - length s) ' ' ++ s
{-
*Main Comp GenTestData Helper Main1 TestConnectionString TestPerf TestPerfOdbc> dump fn1 fn5
>>> warning: large/hdbc prepared                      key missing from left
>>> warning: large/hdbc prepared commit               key missing from left
>>> warning: large/odbc TH                            key missing from left

good:within tolerance 0.1 count=14
medium/hdbc prepared commit Wide             42.04s     43.27s
medium/odbc Raw                              17.57s     17.59s
medium/odbc Raw Commit                       18.35s     18.26s
medium/odbc Raw Commit Wide                  18.08s     18.19s
medium/odbc Raw Wide                         18.03s     18.03s
medium/odbc TH                               17.79s     18.04s
medium/odbc TH Wide                          16.88s     16.92s
small/hdbc prepared commit Wide               4.33s     4.395s
small/odbc Raw                                1.86s     1.903s
small/odbc Raw Commit                        2.054s      2.06s
small/odbc Raw Commit Wide                   1.819s     1.834s
small/odbc Raw Wide                          1.843s      1.81s
small/odbc TH                                1.998s     2.009s
small/odbc TH Wide                           1.606s     1.664s

bad count=6
medium/hdbc prepared                         21.96s     1.776s
medium/hdbc prepared Wide                    2.975s     42.15s
medium/hdbc prepared commit                  1.792s     2.739s
small/hdbc prepared                          2.282s    203.0ms Different measures!
small/hdbc prepared Wide                    370.8ms     4.358s Different measures!
small/hdbc prepared commit                  194.7ms    301.9ms
*Main Comp GenTestData Helper Main1 TestConnectionString TestPerf TestPerfOdbc> dump fn1 fn4
>>> warning: large/hdbc prepared                      key missing from left
>>> warning: large/hdbc prepared Wide                 key missing from left
>>> warning: large/hdbc prepared commit               key missing from left
>>> warning: large/hdbc prepared commit Wide          key missing from left
>>> warning: large/odbc Raw                           key missing from left
>>> warning: large/odbc Raw Commit                    key missing from left
>>> warning: large/odbc Raw Commit Wide               key missing from left
>>> warning: large/odbc Raw Wide                      key missing from left
>>> warning: large/odbc TH                            key missing from left
>>> warning: large/odbc TH Wide                       key missing from left

bad count=20
medium/hdbc prepared                         21.96s    681.6ms Different measures!
medium/hdbc prepared Wide                    2.975s     1.022s
medium/hdbc prepared commit                  1.792s      1.44s
medium/hdbc prepared commit Wide             42.04s     1.804s
medium/odbc Raw                              17.57s    862.3ms Different measures!
medium/odbc Raw Commit                       18.35s     1.303s
medium/odbc Raw Commit Wide                  18.08s     1.839s
medium/odbc Raw Wide                         18.03s     1.305s
medium/odbc TH                               17.79s     1.012s
medium/odbc TH Wide                          16.88s      1.85s
small/hdbc prepared                          2.282s    112.5ms Different measures!
small/hdbc prepared Wide                    370.8ms    154.3ms
small/hdbc prepared commit                  194.7ms    227.0ms
small/hdbc prepared commit Wide               4.33s    240.7ms Different measures!
small/odbc Raw                                1.86s    146.5ms Different measures!
small/odbc Raw Commit                        2.054s    203.8ms Different measures!
small/odbc Raw Commit Wide                   1.819s    238.4ms Different measures!
small/odbc Raw Wide                          1.843s    181.9ms Different measures!
small/odbc TH                                1.998s    148.7ms Different measures!
small/odbc TH Wide                           1.606s    231.1ms Different measures!
*Main Comp GenTestData Helper Main1 TestConnectionString TestPerf TestPerfOdbc>

*Main Comp GenTestData Helper Main1 TestConnectionString TestPerf TestPerfOdbc> dump fn1 fn2
>>> warning: large/odbc TH                            key missing from left

good:within tolerance 0.1 count=20
medium/hdbc prepared                         21.96s      22.0s
medium/hdbc prepared Wide                    2.975s     3.095s
medium/hdbc prepared commit                  1.792s     1.762s
medium/hdbc prepared commit Wide             42.04s     42.04s
medium/odbc Raw                              17.57s     17.64s
medium/odbc Raw Commit                       18.35s     18.28s
medium/odbc Raw Commit Wide                  18.08s     18.25s
medium/odbc Raw Wide                         18.03s     18.06s
medium/odbc TH                               17.79s     17.81s
medium/odbc TH Wide                          16.88s     16.91s
small/hdbc prepared                          2.282s     2.251s
small/hdbc prepared Wide                    370.8ms    348.0ms
small/hdbc prepared commit                  194.7ms    198.4ms
small/hdbc prepared commit Wide               4.33s     4.373s
small/odbc Raw                                1.86s      1.92s
small/odbc Raw Commit                        2.054s     2.035s
small/odbc Raw Commit Wide                   1.819s     1.854s
small/odbc Raw Wide                          1.843s     1.847s
small/odbc TH                                1.998s     1.989s
small/odbc TH Wide                           1.606s     1.567s
-}