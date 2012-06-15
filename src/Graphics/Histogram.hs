{- |

This package easily lets you create high quality histogram plots from your data in Haskell.  It automatically bins your data using whichever binning strategy you'd like, then plots the data.  It uses the gnuplot package to do all the actual graphing, so any options that work for making gnuplot pretty will also work here.

Here's a brief example that should get you going:

>import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
>
>input = [1,0.2,0.23,0.15,0.1,0.88,0.89,0.33,0.05,0.33,0.45,0.99,0.01,0.01,0.5]
>
>simple = do
>    let hist = histogram binSturges input
>    plot "simple.png" hist
>
>advanced = do
>    let hist = histogram binSqrt input
>    let opts = Opts.title "I'm a histogram!" $ 
>               Opts.yLabel "Why?" $ 
>               Opts.xLabel "Because!" $ 
>               defOpts hist
>    plotAdv "advanced.eps" opts hist

-}

module Graphics.Histogram 
    ( 
    
    -- * Creating Histograms
    histogram, histogramBinSize, histogramNumBins,
    
    -- * Binning Strategies
    binSturges, binDoane, binSqrt, binScott, binFreedmanDiaconis,
    
    -- * Graphing Histograms
    PlotOptions, plot, plotAdv, defOpts
    )
-- module Main
    where

import Data.List
import Data.Monoid
import Debug.Trace
import qualified Data.Map as Map

import Numeric

import GHC.IO.Exception

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PostScript as EPS

import qualified Graphics.Gnuplot.Frame.Option as Option
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as Color

-------------------------------------------------------------------------------

-- | Holds all the information needed to plot your histogram.  You shouldn't need to worry about this at all.
data Histogram = Histogram Double Double [(Double,Int)]
--     deriving Show

-------------------------------------------------------------------------------
-- Bin counters; check out http://en.wikipedia.org/wiki/Histogram#Number_of_bins_and_width

type BinningStrat = [Double] -> Int

-- | This is only a helper function to convert strategies that specify bin width into strategies that specify the number of bins
stratFromBinWidth :: [Double] -> Double -> Int
stratFromBinWidth xs h = ceiling $ ((maximum xs) - (minimum xs))/h

-- | Sturges' binning strategy is the least computational work, but recommended for only normal data
binSturges :: BinningStrat
binSturges xs = ceiling $ (log n)/(log 2) + 1
    where n = fromIntegral $ length xs
          
-- | Doane's binning strategy extends Sturges' for non-normal data.  It takes a little more time because it must calculate the kurtosis (peakkiness) of the distribution
binDoane :: BinningStrat
binDoane xs = ceiling $ 1 + (log n) + (log $ 1 + a*((n/6)**(1/2)))
    where 
        n = fromIntegral $ length xs -- :: Double
        a = kurtosis xs -- :: Double
        
-- | Using the sqrt of the number of samples is not supported by any theory, but is commonly used by excell and other histogram making software
binSqrt :: BinningStrat
binSqrt xs = round $ sqrt n
    where 
        n = fromIntegral $ length xs

-- | Scott's rule is the optimal solution for normal data, but requires more computation than Spurges'
binScott :: BinningStrat
binScott xs = stratFromBinWidth xs $ 3.5*(stddev xs) / (n**(1/3))
    where 
        n = fromIntegral $ length xs
        
-- | The Freedman-Diaconis rule is less susceptible to outliers than Scott's and is also used on \"normalish\" data
binFreedmanDiaconis :: BinningStrat
binFreedmanDiaconis xs = stratFromBinWidth xs $ 3.5*(stddev xs) / (n**(1/3))
    where 
        n = fromIntegral $ length xs

-------------------------------------------------------------------------------
-- create the histogram data

-- | Creates a histogram that's ready for plotting.  Call it with one of the binning strategies that is appropriate to the type of data you have.  If you don't know, then try using binSturges.
histogram :: BinningStrat -> [Double] -> Histogram
histogram strat xs = histogramNumBins (strat xs) xs

-- | Create a histogram by specifying the exact bin size. 
-- You probably don't want to use this function, and should use histogram with an appropriate binning strategy.
histogramBinSize :: Double -> [Double] -> Histogram
histogramBinSize size xs = Histogram m s $ fillhist size $ histbin size $ bin size xs
    where
        m = mean xs
        s = stddev xs

-- | Create a histogram by specifying the exact number of bins
-- You probably don't want to use this function, and should use histogram with an appropriate binning strategy.
histogramNumBins :: Int -> [Double] -> Histogram
histogramNumBins n xs =
    histogramBinSize size xs
    where
        size = (fromIntegral $ firstdigit diff) *((10) ** (fromIntegral $ exponent10 diff))
        diff = if diff_test==0
                  then 1
                  else diff_test
        diff_test = ((maximum xs)-(minimum xs))/(fromIntegral n)

        firstdigit dbl = floor $ dbl/((10) ** (fromIntegral $ exponent10 dbl))
        exponent10 dbl = floor $ log10 dbl
        log10 x = (log x) / (log 10)

-- helpers

   -- histbin does all the binning for the histogram
histbin :: Double -> [Double] -> [(Double,Int)]
histbin size xs = Map.toList $ Map.fromList [ (head l, length l) | l <- group (sort xs) ]

   -- histbin bins all the numbers in the histogram, but it ignores any columns with zero elements.
   -- fillhist adds those zero element columns
fillhist :: Double -> [(Double,Int)] -> [(Double,Int)]
fillhist size ((a,b):[]) = [(roundFloat a,b)]
fillhist size ((a,b):xs) = 
    if abs (next-a')<0.0001
       then (roundFloat a,b):(fillhist size xs)
       else (roundFloat a,b):(fillhist size $ (next,0):xs)
    where
        a' = fst $ head xs
        b' = snd $ head xs
        next = roundFloat (a+size)

-- | bin "rounds" every number into the closest number below it that is divisible by size
-- bin :: (Num a, RealFrac a) => a -> [a] -> [a]
bin :: Double -> [Double] -> [Double]
bin size xs = map (\x -> size*(fromIntegral $ floor (x/size))) xs

roundFloat :: Double -> Double
roundFloat num = read $ showFFloat (Just 3) num ""

-------------------------------------------------------------------------------
-- IO

-- | Options for a plot, as specified in the gnuplot library
type PlotOptions = Opts.T (Graph2D.T Int Double)
-- type PlotOptions = Graphics.Gnuplot.Frame.OptionSet.T (Graphics.Gnuplot.Graph.TwoDimensional.T Int Double)

-- | Plots your histogram.  If the filename is empty, then it will open a window and display the histogram on screen.  Otherwise, the filetype is automatically determined by the extension.  Supported file types are .png, .svg (vector graphics), and .eps (PostScript).
plot :: String -> Histogram -> IO GHC.IO.Exception.ExitCode
plot file histdata = plotAdv file (defOpts histdata) histdata

-- | Just like "plot", except you may specify additional options from the gnuplot library.  For example, you could add labels and a title.
plotAdv :: String -> PlotOptions -> Histogram -> IO GHC.IO.Exception.ExitCode
-- plotAdv file opts histdata = Plot.plot (SVG.cons $ file) $ histgen histdata opts
plotAdv file opts histdata = cmd
    where
        n = length file
        cmd = 
            if n==0
                then Plot.plot X11.cons $ histgen histdata opts
                else if (file !! (n-3))=='s' && (file !! (n-2))=='v' && (file !! (n-1))=='g' 
                    then Plot.plot (SVG.cons $ file) $ histgen histdata opts
                    else if (file !! (n-2))=='p' && (file !! (n-1))=='s' 
                        then Plot.plot (EPS.cons $ file) $ histgen histdata opts
                        else Plot.plot (PNG.cons $ file) $ histgen histdata opts
        
-- | Default plot display parameters
defOpts :: Histogram -> PlotOptions
defOpts (Histogram m s xs) = 
--     Opts.add (Option.custom "xtics" "") ["autojustify"] $
    Histogram.clusteredGap 0 $
    OptsStyle.fillBorderLineType (-1) $
    OptsStyle.fillSolid $
    Opts.xTicks2d xlabels $
    Opts.deflt
    where
        xlabels = zip (map (show . fst) xs) [0..]
        
histgen :: Histogram -> Opts.T (Graph2D.T Int Double) -> Frame.T (Graph2D.T Int Double)
histgen (Histogram m s xs) frameOpts =
    Frame.cons frameOpts $
    mconcat $ concat 
        -- this is the bar chart
        [ map (\(title,dat) ->
            fmap (Graph2D.lineSpec (LineSpec.lineColor Color.red $ LineSpec.title title LineSpec.deflt)) $
            Plot2D.list Graph2D.histograms dat) $
                ("", map (fromIntegral . snd) xs) :
                []
                
        -- this is the gaussian
{-        , map (\(title,dat) ->
            fmap (Graph2D.lineSpec (
--                 OptsStyle.custom "smooth" "bezier" $
                LineSpec.title "" $ 
                LineSpec.lineWidth 3 $
                LineSpec.deflt
                )) $
            Plot2D.list Graph2D.listLines dat) $ 
--                 ("", map snd xs):
                ("", map (\(x,y) -> normalscale*(normalpdf m s x)) xs):
--                 ("", replicate 20 20):
                []-}
        ]
    where
        normalscale = (fromIntegral $ maximum $ map snd xs)/(normalpdf m s m)

-- normalcoord m s (x,y) = normalpdf m s x

normalpdf :: Double -> Double -> Double -> Double
normalpdf m s x = (1/(s*(sqrt $ 2*pi)))*(exp $ -(x-m)^2/(2*s^2))

------------------------------------------------------------------------------
-- simple math functions
-- taken from package hstats, which wouldn't fully compile, so I just copied these here

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- |Standard deviation of sample
stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

-- |Sample variance
var :: (Floating a) => [a] -> a
var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)


-- | kurtosis is taken from wikipedia's definition
kurtosis :: (Floating a) => [a] -> a
kurtosis xs = ((1/n) * (sum [(x-x_bar)^4 | x <- xs]))
            / ((1/n) * (sum [(x-x_bar)^2 | x <- xs]))^2 -3
    where 
        n = fromIntegral $ length xs
        x_bar = mean xs

