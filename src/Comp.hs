{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}
module Comp where
import Data.List
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Monad

(<&>) :: (Functor f) => f a -> (a -> b) -> f b
(<&>) = flip fmap

fn1, fn2, fn3, fn4, fn5 :: FilePath
fn1 = "ubuntu1604_virtualbox1_20170724.txt"
fn2 = "ubuntu1604_virtualbox2_20170724.txt"
fn3 = "ubuntu1604_virtualbox_shuffle1_20170725.txt"
fn4 = "windows1_20170724.txt"
fn5 = "ubuntu1604_virtualbox1_HACKAGEHDBC_20170725.txt"

load :: FilePath -> IO (Map String Float)
load fn = do
  xs <- lines <$> readFile fn 
  return $ M.fromListWith (\a b -> error $ "duplicate " ++ show (a,b)) (loadImpl xs)

loadImpl :: [String] -> [(String, Float)]
loadImpl ss =
  let zs = concat $ ss <&> 
             \s -> case words s of
                     "benchmarking":ys -> [Left (unwords ys)]
                     "mean":a:b:_ -> (:[]) . Right . (read a *) $
                                     case b of
                                       "ms" -> 1
                                       "s" -> 1000
                                       _ -> error $ "expected 's' or 'ms' for mean found [" ++ b ++ "]"
                                       
                     _ -> []
     
      ws = take ((length zs `div` 2) * 2) zs
   in chunksOf 2 ws <&> \case 
              [Left a,Right b] -> (a,b)
              o -> error $ "oops expected left right pair only!" ++ show o
                                                 
chunksOf :: Int -> [a] -> [[a]]                   
chunksOf n = unfoldr (\s -> if null s then Nothing else Just $ splitAt n s) 

comp :: FilePath -> FilePath -> IO (Map String Float)
comp f1 f2 = do
  m1 <- load f1
  m2 <- load f2
  forM_ (M.keys (m1 M.\\ m2)) $ \k -> putStrLn $ ">>> warning: key on left side only " ++ k
  forM_ (M.keys (m2 M.\\ m1)) $ \k -> putStrLn $ ">>> warning: key in right side only " ++ k
  return $ M.intersectionWith (/) m1 m2

goodbad :: Float -> Map String Float -> (Map String Float, Map String Float)
goodbad e = M.partition (\a -> a >= 1-e && a <= 1+e)

dump :: FilePath -> FilePath -> IO () 
dump f1 f2 = do
  m <- comp f1 f2 
  let (a,b) = goodbad 0.1 m
  prt "good" a
  prt "bad" b
  
prt :: String -> Map String Float -> IO ()
prt s m | null m = return ()
        | otherwise = do
                        putStrLn $ s ++ " " ++ show (length m)
                        mapM_ print $ M.toList m

{-
*Comp M> dump fn1 fn2
>>> warning: key in right side only large/odbc TH
good 20
("medium/hdbc prepared",0.9981818)
("medium/hdbc prepared Wide",0.9612278)
("medium/hdbc prepared commit",1.0170261)
("medium/hdbc prepared commit Wide",1.0)
("medium/odbc Raw",0.99603176)
("medium/odbc Raw Commit",1.0038294)
("medium/odbc Raw Commit Wide",0.9906849)
("medium/odbc Raw Wide",0.9983389)
("medium/odbc TH",0.99887705)
("medium/odbc TH Wide",0.9982259)
("small/hdbc prepared",1.0137717)
("small/hdbc prepared Wide",1.0655172)
("small/hdbc prepared commit",0.98135084)
("small/hdbc prepared commit Wide",0.99016696)
("small/odbc Raw",0.96875)
("small/odbc Raw Commit",1.0093366)
("small/odbc Raw Commit Wide",0.9811219)
("small/odbc Raw Wide",0.9978343)
("small/odbc TH",1.004525)
("small/odbc TH Wide",1.0248883)

*Comp M> dump fn1 fn4
>>> warning: key in right side only large/hdbc prepared
>>> warning: key in right side only large/hdbc prepared Wide
>>> warning: key in right side only large/hdbc prepared commit
>>> warning: key in right side only large/hdbc prepared commit Wide
>>> warning: key in right side only large/odbc Raw
>>> warning: key in right side only large/odbc Raw Commit
>>> warning: key in right side only large/odbc Raw Commit Wide
>>> warning: key in right side only large/odbc Raw Wide
>>> warning: key in right side only large/odbc TH
>>> warning: key in right side only large/odbc TH Wide
bad 20
("medium/hdbc prepared",32.21831)
("medium/hdbc prepared Wide",2.910959)
("medium/hdbc prepared commit",1.2444445)
("medium/hdbc prepared commit Wide",23.30377)
("medium/odbc Raw",20.37574)
("medium/odbc Raw Commit",14.082886)
("medium/odbc Raw Commit Wide",9.83143)
("medium/odbc Raw Wide",13.816092)
("medium/odbc TH",17.579052)
("medium/odbc TH Wide",9.124325)
("small/hdbc prepared",20.284445)
("small/hdbc prepared Wide",2.4031107)
("small/hdbc prepared commit",0.8577092)
("small/hdbc prepared commit Wide",17.989199)
("small/odbc Raw",12.696246)
("small/odbc Raw Commit",10.078508)
("small/odbc Raw Commit Wide",7.630034)
("small/odbc Raw Wide",10.131941)
("small/odbc TH",13.436449)
("small/odbc TH Wide",6.9493723)

-}