{-# OPTIONS -Wall #-}
module GenTestData where
import Test.QuickCheck
import qualified Test.QuickCheck as Q
import System.IO
import Control.Monad
import Helper
import System.Directory
import Data.List

createHexFiles :: Bool -> Int -> IO ()
createHexFiles forcecreate r = do
  hexFile forcecreate r 4000
  stringFile forcecreate r 2000

hexFile :: Bool -> Int -> Int -> IO ()
hexFile forcecreate r c = do
  let fn = testfnHex r
  b <- doesFileExist fn
  if not b || forcecreate then do
    putStrLn $ "creating " ++ fn
    withBinaryFile fn WriteMode $ \h -> do
      replicateM_ r $ do
        xs <- generate $ Q.vectorOf c $ Q.elements (['0'..'9'] <> ['a'..'f'])
        hPutStrLn h ('0':'x':xs)
  else do
    putStrLn $ "reusing " ++ fn

-- valid are 0-9 a-z A-Z
-- stringFile 100 2000
stringFile :: Bool -> Int -> Int -> IO ()
stringFile forcecreate r c = do
  let fn = testfnString r
  b <- doesFileExist fn
  if not b || forcecreate then do
    putStrLn $ "creating " ++ fn
    withBinaryFile (testfnString r) WriteMode $ \h -> do
      replicateM_ r $ do
        xs <- generate $ Q.vectorOf c $ Q.elements (['0' .. '9'] <> ['a'..'z'] <> ['A' .. 'Z'])
        hPutStr h (xs <> "\n")
  else do
    putStrLn $ "reusing " ++ fn


genTestData :: Int -> IO ()
genTestData nm = do
 putStrLn $ "generating testdata for " ++ show nm ++ " rows"
 let g1 = do
            i <- choose (-100000::Int,100000)
            n <- choose (1,1000)
            s <- vectorOf n (oneof [choose ('a','z'), choose ('0','9')])
            d <- choose (-100000.0::Double,100000.0)
            return (i,s,d)
 withFile (testfn nm) WriteMode $ \h -> do
   replicateM_ nm $ do
     (i,s,d) <- generate g1
     hPutStrLn h $ show i ++ "\t" ++ s ++ "\t" ++ show d
 putStrLn $ "generated testdata for " ++ show nm ++ " rows"

genTestDataWide :: Int -> IO ()
genTestDataWide nm = do
 putStrLn $ "generating wide testdata for " ++ show nm ++ " rows"
 let g1 = do
            is <- replicateM 10 $ choose (-100000::Int,100000)
            n <- choose (100,200)
            ss <- replicateM 10 $ vectorOf n (oneof [choose ('a','z'), choose ('0','9')])
            return (is,ss)
 withFile (testfnWide nm) WriteMode $ \h -> do
   replicateM_ nm $ do
     (ii,ss) <- generate g1
     hPutStrLn h $ intercalate "\t" (map show ii ++ ss)
 putStrLn $ "generated wide testdata for " ++ show nm ++ " rows"

createTestFileIfNotExist, createTestFileWideIfNotExist :: Int -> IO FilePath
createTestFileIfNotExist = createTestFileIfNotExist' True
createTestFileWideIfNotExist = createTestFileIfNotExist' False

createTestFileIfNotExist' :: Bool -> Int -> IO FilePath
createTestFileIfNotExist' normal i = do
  let fn = if normal then testfn i else testfnWide i
  b <- doesFileExist fn
  unless b $ do
    putStrLn $ "generating " ++ fn
    if normal then genTestData i else genTestDataWide i
    putStrLn $ "generated " ++ fn
  return fn
