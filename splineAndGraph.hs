module TestSpline where
import Data.Array
import Graphics.EasyPlot

x1 :: Array Integer Double
x1 = array(0,3) [(0, -2),(1, 1), (2, 3), (3, 5)]
y1 = array(0, 3) [(0, 0), (1, 1.9), (2,-1.2), (3, -1.6)]
m1 = array(0, 3) [(0, 0.125882), (1, 1.24706), (2, -1.36203), (3, 0.125882)]



splinesum m0 m1 x0 x1 y0 y1 x = term1 + term2 + term3 + term4
    where
    term1 = m0*((x1-x)**3)/(6*h)
    term2 = m1*((x - x0)**3)/(6*h)
    term3 = (y0 - (m0 * h**2)/6)*(x1 - x)/h
    term4 =  (y1 - (m1 * h**2)/6)*(x - x0)/h
    h = x1 - x0

spline4Points :: (Array Integer Double) -> (Array Integer Double) -> (Array Integer Double) -> Double -> Double
spline4Points dataX dataY dataM x
  | x0 <= x && x1 >= x  = splinesum (dataM ! 0) (dataM ! 1) x0 x1 (dataY ! 0) (dataY ! 1) x
  | x1 <= x && x2 >= x  = splinesum (dataM ! 1) (dataM ! 2) x1 x2 (dataY ! 1) (dataY ! 2) x   
  | x2 <= x && x3 >= x  = splinesum (dataM ! 2) (dataM ! 3) x2 x3 (dataY ! 2) (dataY ! 3) x   
    
    where
      x0 = dataX ! 0
      x1 = dataX ! 1
      x2 = dataX ! 2
      x3 = dataX ! 3

spline5Points :: (Array Integer Double) -> (Array Integer Double) -> (Array Integer Double) -> Double -> Double
spline5Points dataX dataY dataM x
  | x0 <= x && x1 >= x  = splinesum (dataM ! 0) (dataM ! 1) x0 x1 (dataY ! 0) (dataY ! 1) x
  | x1 <= x && x2 >= x  = splinesum (dataM ! 1) (dataM ! 2) x1 x2 (dataY ! 1) (dataY ! 2) x   
  | x2 <= x && x3 >= x  = splinesum (dataM ! 2) (dataM ! 3) x2 x3 (dataY ! 2) (dataY ! 3) x   
  | x3 <= x && x4 >= x  = splinesum (dataM ! 3) (dataM ! 4) x3 x4 (dataY ! 3) (dataY ! 4) x  
    where
      x0 = dataX ! 0
      x1 = dataX ! 1
      x2 = dataX ! 2
      x3 = dataX ! 3
      x4 = dataX ! 4

main = do
  plot  (PNG "spline1.png") [Data2D [Title "Interpolation Points", Color Green] [Range (-2) 5] dataxy1, Function2D [Title "Spline", Color Blue] [Range (-2) 5] (\x -> spline4Points x1 y1 m1 x)]
  --plot (PNG "spline2.png") [Data2D [Title "Interpolation Points", Color Green] [Range 0 5] dataxy2, Function2D [Title "Spline", Color Blue] [Range 0 5] (\x -> spline5Points x2 y2 m2 x)]
  --plot X11 [ Data2D [Title "Graph 1", Color Red] [] [(x, x ** 3) | x <- [-4,-3.9..4]]
    --       , Function2D [Title "Function 2", Color Blue] [] (\x -> negate $ x ** 2) ]
  where
    dataxy1 = [(-2,0),(1,1.9),(3,-1.2),(5,-1.6)]
--    dataxy2 = [(0,0.0), (1, 0.84147), (2.5, 0.59847), (3.6, -0.44252), (5, -0.95892)]
