module AR (ar) where

import Control.Monad.Writer
import System.Random (StdGen, mkStdGen, randomR)

type A = ([Double], [Double], StdGen)
type AR = Writer [Double] A

bound :: (Double, Double)
bound = (-0.1, 0.1)

compute :: A -> AR
compute (an, xs, g) = do
  let (r, g') = randomR bound g
      x = (r+) $ sum $ zipWith (*) an xs
  tell [x]
  return (an, (x:) $ init xs, g')

computeN :: Int -> A -> AR
computeN 0 a = return a
computeN n a' = do
  a <- compute a'
  computeN (n-1) a

ar :: Int -> [Double] -> Int -> [Double]
ar times an initGen = out
  where
    initVal = (an, replicate (length an) 0, mkStdGen initGen)
    (_, out) = runWriter $ computeN times initVal
