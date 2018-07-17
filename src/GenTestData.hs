{-# OPTIONS -Wall #-}
module GenTestData where
import Test.QuickCheck
import System.IO
import Control.Monad
import Helper

genTestData :: Int -> IO ()
genTestData nm = do
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

