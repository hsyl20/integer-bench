-- {-# OPTIONS_GHC -ddump-simpl #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}

import Criterion.Main

import qualified Haskus.Number.BigNat as H
import qualified Haskus.Number.Natural as H
import qualified GHC.Integer.Simple as S
import qualified GHC.Integer.Simple.Type as S
import GHC.Natural
import GHC.Exts
import Data.Bits
import Data.Maybe

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks = 
   let
      wSmall1, wSmall2 :: Word
      !wSmall1 = 0x123456
      !wSmall2 = 0x789ABC

      bnSmall1,bnSmall2 :: H.BigNat
      !bnSmall1 = fromIntegral wSmall1
      !bnSmall2 = fromIntegral wSmall2

      hnSmall1,hnSmall2 :: H.Natural
      !hnSmall1 = fromIntegral wSmall1
      !hnSmall2 = fromIntegral wSmall2

      nSmall1,nSmall2 :: Natural
      !nSmall1 = fromIntegral wSmall1
      !nSmall2 = fromIntegral wSmall2

      sSmall1,sSmall2 :: S.Integer
      !sSmall1 = case wSmall1 of W# w -> S.wordToInteger w
      !sSmall2 = case wSmall2 of W# w -> S.wordToInteger w

      iBig1,iBig2 :: Integer
      !iBig1 = fromIntegral wSmall1 `shiftL` 5000 + fromIntegral wSmall1
      !iBig2 = fromIntegral wSmall2 `shiftL` 5000 + fromIntegral wSmall2

      bnBig1,bnBig2 :: H.BigNat
      !bnBig1 = fromIntegral iBig1
      !bnBig2 = fromIntegral iBig2

      hnBig1,hnBig2 :: H.Natural
      !hnBig1 = fromIntegral iBig1
      !hnBig2 = fromIntegral iBig2

      nBig1,nBig2 :: Natural
      !nBig1 = fromIntegral iBig1
      !nBig2 = fromIntegral iBig2

      sBig1,sBig2 :: S.Integer
      !sBig1 = (sSmall1 `S.shiftLInteger` 5000#) `S.plusInteger` sSmall1
      !sBig2 = (sSmall2 `S.shiftLInteger` 5000#) `S.plusInteger` sSmall2

      mSpl :: a -> Maybe a
      mSpl = const Nothing
      --mSpl = Just

      mBgn :: a -> Maybe a
      mBgn = const Nothing
      --mBgn = Just


   in
   [ bgroup "Small addition" $ catMaybes
      [ mBgn $ bench "Haskus BigNat"    $ whnf (uncurry (+))           $! (bnSmall1,bnSmall2)
      , Just $ bench "Haskus Natural"   $ whnf (uncurry (+))           $! (hnSmall1,hnSmall2)
      , Just $ bench "Natural"          $ whnf (uncurry (+))           $! (nSmall1,nSmall2)
      , mSpl $ bench "Simple integer"   $ whnf (uncurry S.plusInteger) $! (sSmall1,sSmall2)
      ]
   , bgroup "Small multiplication" $ catMaybes
      [ mBgn $ bench "Haskus BigNat"    $ whnf (uncurry (*))            $! (bnSmall1,bnSmall2)
      , Just $ bench "Haskus Natural"   $ whnf (uncurry (*))            $! (hnSmall1,hnSmall2)
      , Just $ bench "Natural"          $ whnf (uncurry (*))            $! (nSmall1,nSmall2)
      , mSpl $ bench "Simple integer"   $ whnf (uncurry S.timesInteger) $! (sSmall1,sSmall2)
      ]
   , bgroup "Small quotRem" $ catMaybes
      [ mBgn $ bench "Haskus BigNat"    $ whnf (uncurry quotRem)           $! (bnSmall1,bnSmall2)
      , Just $ bench "Haskus Natural"   $ whnf (uncurry quotRem)           $! (hnSmall1,hnSmall2)
      , Just $ bench "Natural"          $ whnf (uncurry quotRem)           $! (nSmall1,nSmall2)
      , mSpl $ bench "Simple integer"   $ whnf (uncurry S.divModInteger')  $! (sSmall1,sSmall2)
      ]
   , bgroup "Big addition" $ catMaybes
      [ mBgn $ bench "Haskus BigNat"    $ whnf (uncurry (+))            $! (bnBig1,bnBig2)
      , Just $ bench "Haskus Natural"   $ whnf (uncurry (+))            $! (hnBig1,hnBig2)
      , Just $ bench "Natural"          $ whnf (uncurry (+))            $! (nBig1,nBig2)
      , mSpl $ bench "Simple integer"   $ whnf (uncurry S.plusInteger)  $! (sBig1,sBig2)
      ]
   , bgroup "Big multiplication" $ catMaybes
      [ mBgn $ bench "Haskus BigNat"    $ whnf (uncurry (*))            $! (bnBig1,bnBig2)
      , Just $ bench "Haskus Natural"   $ whnf (uncurry (*))            $! (hnBig1,hnBig2)
      , Just $ bench "Natural"          $ whnf (uncurry (*))            $! (nBig1,nBig2)
      , mSpl $ bench "Simple integer"   $ whnf (uncurry S.timesInteger) $! (sBig1,sBig2)
      ]
   , bgroup "Big quotRem" $ catMaybes
      [ mBgn $ bench "Haskus BigNat"    $ whnf (uncurry quotRem)           $! (bnBig1,bnBig2)
      , Just $ bench "Haskus Natural"   $ whnf (uncurry quotRem)           $! (hnBig1,hnBig2)
      , Just $ bench "Natural"          $ whnf (uncurry quotRem)           $! (nBig1,nBig2)
      , mSpl $ bench "Simple integer"   $ whnf (uncurry S.divModInteger')  $! (sBig1,sBig2)
      ]
   ]
