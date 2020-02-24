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
   [ "Time:      ", show (avgTime / avgIters)
   ]
   where
      pad = replicate (3*off) ' '
      avgTime  = sum (fmap measTime ms)
      avgIters = sum (fmap (fromIntegral . measIters) ms)


benchmarks :: [Benchmark]
benchmarks =
   [ bgroup "Small" $
      [ bgroup "addition" $
         [ bench "Word"    $ whnf (uncurry (+)) $! (vWord1, vWord2)
         , bench "Int"     $ whnf (uncurry (+)) $! (vInt1, vInt2)
         , bench "Integer" $ whnf (uncurry (+)) $! (vSmallInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry (+)) $! (vSmallNatural1, vSmallNatural2)
         ]
      , bgroup "multiplication" $
         [ bench "Word"    $ whnf (uncurry (*)) $! (vWord1, vWord2)
         , bench "Int"     $ whnf (uncurry (*)) $! (vInt1, vInt2)
         , bench "Integer" $ whnf (uncurry (*)) $! (vSmallInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry (*)) $! (vSmallNatural1, vSmallNatural2)
         ]
      , bgroup "subtraction" $
         [ bench "Word"    $ whnf (uncurry (-)) $! (vWord1, vWord2)
         , bench "Int"     $ whnf (uncurry (-)) $! (vInt1, vInt2)
         , bench "Integer" $ whnf (uncurry (-)) $! (vSmallInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry (-)) $! (vSmallNatural1, vSmallNatural2)
         ]
      , bgroup "division" $
         [ bench "Word"    $ whnf (uncurry div) $! (vWord1, vWord2)
         , bench "Int"     $ whnf (uncurry div) $! (vInt1, vInt2)
         , bench "Integer" $ whnf (uncurry div) $! (vSmallInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry div) $! (vSmallNatural1, vSmallNatural2)
         ]
      , bgroup "quotRem" $
         [ bench "Word"    $ whnf (uncurry quotRem) $! (vWord1, vWord2)
         , bench "Int"     $ whnf (uncurry quotRem) $! (vInt1, vInt2)
         , bench "Integer" $ whnf (uncurry quotRem) $! (vSmallInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry quotRem) $! (vSmallNatural1, vSmallNatural2)
         ]
      , bgroup "gcd" $
         [ bench "Word"    $ whnf (uncurry gcd) $! (vWord1, vWord2)
         , bench "Int"     $ whnf (uncurry gcd) $! (vInt1, vInt2)
         , bench "Integer" $ whnf (uncurry gcd) $! (vSmallInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry gcd) $! (vSmallNatural1, vSmallNatural2)
         ]
      , bgroup "lcm" $
         [ bench "Word"    $ whnf (uncurry lcm) $! (vWord1, vWord2)
         , bench "Int"     $ whnf (uncurry lcm) $! (vInt1, vInt2)
         , bench "Integer" $ whnf (uncurry lcm) $! (vSmallInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry lcm) $! (vSmallNatural1, vSmallNatural2)
         ]
      ]
   , bgroup "Medium" $
      [ bgroup "addition" $
         [ bench "Integer" $ whnf (uncurry (+)) $! (vMediumInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry (+)) $! (vMediumNatural1, vMediumNatural2)
         ]
      , bgroup "multiplication" $
         [ bench "Integer" $ whnf (uncurry (*)) $! (vMediumInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry (*)) $! (vMediumNatural1, vMediumNatural2)
         ]
      , bgroup "subtraction" $
         [ bench "Integer" $ whnf (uncurry (-)) $! (vMediumInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry (-)) $! (vMediumNatural1, vMediumNatural2)
         ]
      , bgroup "division" $
         [ bench "Integer" $ whnf (uncurry div) $! (vMediumInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry div) $! (vMediumNatural1, vMediumNatural2)
         ]
      , bgroup "quotRem" $
         [ bench "Integer" $ whnf (uncurry quotRem) $! (vMediumInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry quotRem) $! (vMediumNatural1, vMediumNatural2)
         ]
      , bgroup "gcd" $
         [ bench "Integer" $ whnf (uncurry gcd) $! (vMediumInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry gcd) $! (vMediumNatural1, vMediumNatural2)
         ]
      , bgroup "lcm" $
         [ bench "Integer" $ whnf (uncurry lcm) $! (vMediumInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry lcm) $! (vMediumNatural1, vMediumNatural2)
         ]
      ]
   , bgroup "Big" $
      [ bgroup "addition" $
         [ bench "Integer" $ whnf (uncurry (+)) $! (vBigInteger1, vBigInteger2)
         , bench "Natural" $ whnf (uncurry (+)) $! (vBigNatural1, vBigNatural2)
         ]
      , bgroup "multiplication" $
         [ bench "Integer" $ whnf (uncurry (*)) $! (vBigInteger1, vBigInteger2)
         , bench "Natural" $ whnf (uncurry (*)) $! (vBigNatural1, vBigNatural2)
         ]
      , bgroup "subtraction" $
         [ bench "Integer" $ whnf (uncurry (-)) $! (vBigInteger1, vBigInteger2)
         , bench "Natural" $ whnf (uncurry (-)) $! (vBigNatural1, vBigNatural2)
         ]
      , bgroup "division" $
         [ bench "Integer" $ whnf (uncurry div) $! (vBigInteger1, vBigInteger2)
         , bench "Natural" $ whnf (uncurry div) $! (vBigNatural1, vBigNatural2)
         ]
      , bgroup "quotRem" $
         [ bench "Integer" $ whnf (uncurry quotRem) $! (vBigInteger1, vBigInteger2)
         , bench "Natural" $ whnf (uncurry quotRem) $! (vBigNatural1, vBigNatural2)
         ]
      , bgroup "gcd" $
         [ bench "Integer" $ whnf (uncurry gcd) $! (vBigInteger1, vBigInteger2)
         , bench "Natural" $ whnf (uncurry gcd) $! (vBigNatural1, vBigNatural2)
         ]
      , bgroup "lcm" $
         [ bench "Integer" $ whnf (uncurry lcm) $! (vBigInteger1, vBigInteger2)
         , bench "Natural" $ whnf (uncurry lcm) $! (vBigNatural1, vBigNatural2)
         ]
      ]
   , bgroup "Big-small" $
      [ bgroup "addition" $
         [ bench "Integer" $ whnf (uncurry (+)) $! (vBigInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry (+)) $! (vBigNatural1, vSmallNatural2)
         ]
      , bgroup "multiplication" $
         [ bench "Integer" $ whnf (uncurry (*)) $! (vBigInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry (*)) $! (vBigNatural1, vSmallNatural2)
         ]
      , bgroup "subtraction" $
         [ bench "Integer" $ whnf (uncurry (-)) $! (vBigInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry (-)) $! (vBigNatural1, vSmallNatural2)
         ]
      , bgroup "division" $
         [ bench "Integer" $ whnf (uncurry div) $! (vBigInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry div) $! (vBigNatural1, vSmallNatural2)
         ]
      , bgroup "quotRem" $
         [ bench "Integer" $ whnf (uncurry quotRem) $! (vBigInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry quotRem) $! (vBigNatural1, vSmallNatural2)
         ]
      , bgroup "gcd" $
         [ bench "Integer" $ whnf (uncurry gcd) $! (vBigInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry gcd) $! (vBigNatural1, vSmallNatural2)
         ]
      , bgroup "lcm" $
         [ bench "Integer" $ whnf (uncurry lcm) $! (vBigInteger1, vSmallInteger2)
         , bench "Natural" $ whnf (uncurry lcm) $! (vBigNatural1, vSmallNatural2)
         ]
      ]
   , bgroup "Big-medium" $
      [ bgroup "addition" $
         [ bench "Integer" $ whnf (uncurry (+)) $! (vBigInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry (+)) $! (vBigNatural1, vMediumNatural2)
         ]
      , bgroup "multiplication" $
         [ bench "Integer" $ whnf (uncurry (*)) $! (vBigInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry (*)) $! (vBigNatural1, vMediumNatural2)
         ]
      , bgroup "subtraction" $
         [ bench "Integer" $ whnf (uncurry (-)) $! (vBigInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry (-)) $! (vBigNatural1, vMediumNatural2)
         ]
      , bgroup "division" $
         [ bench "Integer" $ whnf (uncurry div) $! (vBigInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry div) $! (vBigNatural1, vMediumNatural2)
         ]
      , bgroup "quotRem" $
         [ bench "Integer" $ whnf (uncurry quotRem) $! (vBigInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry quotRem) $! (vBigNatural1, vMediumNatural2)
         ]
      , bgroup "gcd" $
         [ bench "Integer" $ whnf (uncurry gcd) $! (vBigInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry gcd) $! (vBigNatural1, vMediumNatural2)
         ]
      , bgroup "lcm" $
         [ bench "Integer" $ whnf (uncurry lcm) $! (vBigInteger1, vMediumInteger2)
         , bench "Natural" $ whnf (uncurry lcm) $! (vBigNatural1, vMediumNatural2)
         ]
      ]
   ]
