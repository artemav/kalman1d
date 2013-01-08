module Charts (charts
              ,module Graphics.Rendering.Chart
              ,module Graphics.Rendering.Chart.Gtk)
       where

import qualified AR
import qualified KF
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Gtk
import Data.Colour.Names
import Data.Colour
import Data.Accessor

aCoeffs :: [Double]
aCoeffs = [-0.02 -- a1, ...
          , 0.1
          , 0.008
          , 0.003
          , -0.4
          , 0.035
          , 0.001
          , -0.58]

charts :: Layout1 Int Double
charts = layout
  where
    ar = AR.ar 100 aCoeffs 1
    sin_or = [sin $ fromIntegral x | x <- [0 .. length ar]]
    sin_ar = zipWith (+) ar sin_or

    sinOrig = plot_lines_values ^= [zip [x | x <- [0 .. length sin_or]] sin_or]
              $ plot_lines_style  .> line_color ^= opaque yellow
              $ plot_lines_title ^= "Sin"
              $ defaultPlotLines

    autoreg = plot_lines_values ^= [zip [x | x <- [0 .. length sin_ar]] sin_ar]
              $ plot_lines_style  .> line_color ^= opaque red
              $ plot_lines_title ^= "Autoregression"
              $ defaultPlotLines

    kf = KF.kf sin_ar 4
    kalmanf = plot_lines_values ^= [zip [x | x <- [0 .. length kf]] kf]
              $ plot_lines_title ^= "Kalman filter"
              $ defaultPlotLines

    layout = layout1_title ^= "Artem Artemiev"
             $ layout1_plots ^= [Left (toPlot kalmanf)
                                ,Left (toPlot sinOrig)
                                ,Left (toPlot autoreg)]
             $ defaultLayout1
