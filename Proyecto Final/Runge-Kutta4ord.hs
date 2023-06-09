

func :: ( Double , Double ) -> Double
func (x , t) = -x ^3 + sin t -- la f (x , y )

ini :: Double
ini = 0.0

fin :: Double
fin = 10.0

pasos :: Int
pasos = 1000

h :: Double -> Double -> Double -> Double
h c a ps = (c-a)/ps

x0 :: Double
x0 = 0.0

y0 :: Double
y0 = 0.0

{-Cuadros de Butcher para los diversos algoritmos de 
Runge-Kutta -}

-- Algoritmo Runge-Kurra de orden 4 
rK4 :: [[Double]]
rK4 = [[0, 0],
       [0.5, 0.5],
       [0.5, 0, 0.5],
       [1, 0, 0, 1]]

brK4 :: [Double]
brK4 = [1/6, 1/3, 1/3, 1/6]

-- Regla de los 3/8 de Kutta  
r3d8  :: [[Double]]
r3d8 = [[0, 0],
       [ 1/3, 1/3],
       [2/3, -1/3, 1],
       [1, 1, -1, 1]]

br3d8 :: [Double]
br3d8 = [1/8, 3/8, 3/8, 1/8]

beuler :: [Double]
beuler = [1]

{--}
kTermRK ::  ((Double, Double)-> Double) -> [[Double]] -> Double -> (Double, Double) -> Int -> Double
kTermRK f but ph (x, t) m
 | m == 1 = f (x, t) 
 | otherwise = f(xStep, tStep)
            where 
                tStep = t + ((but !! (m-1)) !! 0)* ph
                xStep = x + sum (zipWith (*) cns ks) * ph
                      where
                        cns = drop 1 (but !! (m-1))
                        ks = map (kTermRK f but ph (x, t))  [1 .. m-1]

                

rungeKutta ::  ((Double, Double)-> Double) -> [[Double]] -> [Double] ->  Double -> (Double, Double) -> Int -> (Double, Double)             
rungeKutta f but b ph (x, t)  m = (x', t')
                             where
                                   x' = x + ph * sum ( zipWith (*) b (map (kTermRK f but ph (x, t))  [1 .. m]))
                                   t' = t + ph

