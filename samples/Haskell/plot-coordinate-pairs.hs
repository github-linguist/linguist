import Graphics.Gnuplot.Simple

pnts = [2.7, 2.8, 31.4, 38.1, 58.0, 76.2, 100.5, 130.0, 149.3, 180.0]

doPlot = plotPathStyle [ ( Title "plotting dots" )]
            (PlotStyle Points (CustomStyle []))  (zip [0..] pnts)
