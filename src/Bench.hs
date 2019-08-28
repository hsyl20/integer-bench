-- {-# OPTIONS_GHC -ddump-simpl #-}
{-# LANGUAGE MagicHash #-}

import Criterion.Main

import qualified Haskus.Natural as H
import qualified GHC.Integer.Simple as S
import qualified GHC.Integer.Simple.Type as S
import GHC.Natural
import GHC.Exts
import Data.Bits

main :: IO ()
main = defaultMain benchmarks

wSmall1, wSmall2 :: Word
wSmall1 = 0x123456
wSmall2 = 0x789ABC

hSmall1,hSmall2 :: H.Natural
hSmall1 = H.naturalFromWord wSmall1
hSmall2 = H.naturalFromWord wSmall2

nSmall1,nSmall2 :: Natural
nSmall1 = fromIntegral wSmall1
nSmall2 = fromIntegral wSmall2

sSmall1,sSmall2 :: S.Integer
sSmall1 = case wSmall1 of W# w -> S.wordToInteger w
sSmall2 = case wSmall2 of W# w -> S.wordToInteger w

hBig1,hBig2 :: H.Natural
hBig1 = hSmall1 `shiftL` 5000 + hSmall1
hBig2 = hSmall2 `shiftL` 5000 + hSmall2

nBig1,nBig2 :: Natural
nBig1 = nSmall1 `shiftL` 5000 + nSmall1
nBig2 = nSmall2 `shiftL` 5000 + nSmall2

sBig1,sBig2 :: S.Integer
sBig1 = (sSmall1 `S.shiftLInteger` 5000#) `S.plusInteger` sSmall1
sBig2 = (sSmall2 `S.shiftLInteger` 5000#) `S.plusInteger` sSmall2

benchmarks :: [Benchmark]
benchmarks = 
   [ bgroup "Small addition"
      [ bench "Haskus"           $ whnf (uncurry (+)) (hSmall1,hSmall2)
      , bench "Natural"          $ whnf (uncurry (+)) (nSmall1,nSmall2)
      , bench "Simple integer"   $ whnf (uncurry S.plusInteger) (sSmall1,sSmall2)
      ]
   , bgroup "Small multiplication"
      [ bench "Haskus"           $ whnf (uncurry (*)) (hSmall1,hSmall2)
      , bench "Natural"          $ whnf (uncurry (*)) (nSmall1,nSmall2)
      , bench "Simple integer"   $ whnf (uncurry S.timesInteger) (sSmall1,sSmall2)
      ]
   , bgroup "Small quotRem"
      [ bench "Haskus"           $ whnf (uncurry quotRem) (hSmall1,hSmall2)
      , bench "Natural"          $ whnf (uncurry quotRem) (nSmall1,nSmall2)
      , bench "Simple integer"   $ whnf (uncurry S.divModInteger') (sSmall1,sSmall2)
      ]
   , bgroup "Big addition"
      [ bench "Haskus"           $ whnf (uncurry (+)) (hBig1,hBig2)
      , bench "Natural"          $ whnf (uncurry (+)) (nBig1,nBig2)
      , bench "Simple integer"   $ whnf (uncurry S.plusInteger) (sBig1,sBig2)
      ]
   , bgroup "Big multiplication"
      [ bench "Haskus"           $ whnf (uncurry (*)) (hBig1,hBig2)
      , bench "Natural"          $ whnf (uncurry (*)) (nBig1,nBig2)
      , bench "Simple integer"   $ whnf (uncurry S.timesInteger) (sBig1,sBig2)
      ]
   , bgroup "Big quotRem"
      [ bench "Haskus"           $ whnf (uncurry quotRem) (hBig1,hBig2)
      , bench "Natural"          $ whnf (uncurry quotRem) (nBig1,nBig2)
      , bench "Simple integer"   $ whnf (uncurry S.divModInteger') (sBig1,sBig2)
      ]
   ]
