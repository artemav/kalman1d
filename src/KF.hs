module KF (kf) where

import Control.Monad.Writer
import System.Random (StdGen, mkStdGen, randomR)

r :: Double
r = 1e-6

q :: Double
q = 1e-5

h :: Double
h = 1

a :: Double
a = 1

type K = (Double, Double, StdGen)
type KF = Writer [Double] K

computeN :: K -> [Double] -> KF
computeN k [] = return k
computeN k' (z:zs) =
  do k <- compute k' z
     computeN k zs

bound :: (Double, Double)
bound = (-0.1, 0.1)

compute :: K -> Double -> KF
compute (x', p', g') z =
  do let (accident, g) = randomR bound g'
         x'' = a*x' + accident
         p'' = a*p' + q
         y = z - h*x''
         s = h*p'' + r
         k = p''/s
         x = p'' + k*y
         p = (1 - k*h)*p''
     tell [x]
     return (x, p, g)

kf :: [Double] -> Int -> [Double]
kf ar g = out
  where (_, out) = runWriter $ computeN (0, 1, mkStdGen g) ar
