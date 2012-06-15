module Main
    where

import Graphics.Histogram

import System.Random
import Control.Monad.Random

import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

randomList :: (RandomGen g) => (Double,Double) -> Int -> Rand g [Double]
randomList r n = sequence $ replicate n $ getRandomR r

main=adv
simple = do
    let input = [1,0.2,0.23,0.15,0.1,0.88,0.89,0.33,0.05,0.33,0.45,0.99,0.01,0.01,0.5]
    let hist = histogram binSturges input
    plot "test.eps" hist

adv = do
    let input = [1,0.2,0.23,0.15,0.1,0.88,0.89,0.33,0.05,0.33,0.45,0.99,0.01,0.01,0.5]
    let hist = histogram binSqrt input
    let opts = Opts.title "I'm a histogram!" $ 
               Opts.yLabel "Why?" $ 
               Opts.xLabel "Because!" $ 
               defOpts hist
    plotAdv "test.png" opts hist
    
test = do
    let m=10
    let bin=10
    let num=10000
    
    xs <- evalRandIO $ randomList (0,m) num
    ys <- evalRandIO $ randomList (0,m) num
    let zs = [ x+y | (x,y) <- zip xs ys]
    let hist=histogram binSturges zs
--     putStrLn $ show $ hist

    plot "test.eps" hist