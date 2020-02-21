{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

import Criterion.Measurement
import Criterion.Measurement.Types

import BenchValues
import Control.Monad

main :: IO ()
main = runBench 0 benchmarks
   
class RunBench a where
   runBench :: Int -> a -> IO ()

instance RunBench [Benchmark] where
   runBench off benchs = forM_ benchs (runBench off)

instance RunBench Benchmark where
   runBench off (Environment setEnv unsetEnv b) = do
      e <- setEnv
      r <- runBench off (b e)
      _ <- unsetEnv e
      return r
   runBench off (Benchmark name b) = do
      indentName off name
      let lowerBound = 2
      (measures,_totalTime) <- runBenchmark b lowerBound 
      putStrLn (showMeasures (off+1) measures)
   runBench off (BenchGroup name benchs) = do
      indentName off name
      forM_ benchs (runBench (off + 1))

indentName :: Int -> String -> IO ()
indentName n s = putStrLn (replicate (3*n) ' ' ++ "+ "++ s)

showMeasures :: Int -> [Measured] -> String
showMeasures off ms = mconcat $ fmap (pad ++)
   [ "Time:      ", show avgTime
   ]
   where
      pad = replicate (3*off) ' '
      n = length ms
      avgTime = sum (fmap measTime ms) / fromIntegral n


benchmarks :: [Benchmark]
benchmarks =
   [ bgroup "Small addition" $
      [ bench "Word"    $ whnf (uncurry (+)) $! (vWord1, vWord2)
      , bench "Int"     $ whnf (uncurry (+)) $! (vInt1, vInt2)
      , bench "Integer" $ whnf (uncurry (+)) $! (vSmallInteger1, vSmallInteger2)
      , bench "Natural" $ whnf (uncurry (+)) $! (vSmallNatural1, vSmallNatural2)
      ]
   , bgroup "Small multiplication" $
      [ bench "Word"    $ whnf (uncurry (*)) $! (vWord1, vWord2)
      , bench "Int"     $ whnf (uncurry (*)) $! (vInt1, vInt2)
      , bench "Integer" $ whnf (uncurry (*)) $! (vSmallInteger1, vSmallInteger2)
      , bench "Natural" $ whnf (uncurry (*)) $! (vSmallNatural1, vSmallNatural2)
      ]
   , bgroup "Medium addition" $
      [ bench "Integer" $ whnf (uncurry (+)) $! (vMediumInteger1, vMediumInteger2)
      , bench "Natural" $ whnf (uncurry (+)) $! (vMediumNatural1, vMediumNatural2)
      ]
   , bgroup "Medium multiplication" $
      [ bench "Integer" $ whnf (uncurry (*)) $! (vMediumInteger1, vMediumInteger2)
      , bench "Natural" $ whnf (uncurry (*)) $! (vMediumNatural1, vMediumNatural2)
      ]
   , bgroup "Medium subtraction" $
      [ bench "Integer" $ whnf (uncurry (-)) $! (vMediumInteger1, vMediumInteger2)
      , bench "Natural" $ whnf (uncurry (-)) $! (vMediumNatural1, vMediumNatural2)
      ]
   , bgroup "Medium division" $
      [ bench "Integer" $ whnf (uncurry div) $! (vMediumInteger1, vMediumInteger2)
      , bench "Natural" $ whnf (uncurry div) $! (vMediumNatural1, vMediumNatural2)
      ]
   , bgroup "Big addition" $
      [ bench "Integer" $ whnf (uncurry (+)) $! (vBigInteger1, vBigInteger2)
      , bench "Natural" $ whnf (uncurry (+)) $! (vBigNatural1, vBigNatural2)
      ]
   , bgroup "Big multiplication" $
      [ bench "Integer" $ whnf (uncurry (*)) $! (vBigInteger1, vBigInteger2)
      , bench "Natural" $ whnf (uncurry (*)) $! (vBigNatural1, vBigNatural2)
      ]
   , bgroup "Big subtraction" $
      [ bench "Integer" $ whnf (uncurry (-)) $! (vBigInteger1, vBigInteger2)
      , bench "Natural" $ whnf (uncurry (-)) $! (vBigNatural1, vBigNatural2)
      ]
   , bgroup "Big division" $
      [ bench "Integer" $ whnf (uncurry div) $! (vBigInteger1, vBigInteger2)
      , bench "Natural" $ whnf (uncurry div) $! (vBigNatural1, vBigNatural2)
      ]
   ]
