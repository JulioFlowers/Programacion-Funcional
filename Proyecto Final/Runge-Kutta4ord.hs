

{-

PROYECTO FINAL: TSFC2 Programación Funcional

ALGORITMO GENERAL DEL METODO EXPLICITO DE RUNGE - KUTTA

-Perez Flores Julio Alfonso (julio_perez@ciencias.unam.mx)
-Facultad de Ciencias, UNAM.
-Junio 8, 2023

DESCRIPCIÓN: Este proyecto utiliza diferentes conceptos de 
programación vistos en el curso de TSFC II como son:
 
-funciones de alto nivel,
-calculo lambda,
-funciones curreadas 
-recursion.

Con la finalidad de implementar el metodo general explicito
de integración numerica de Runge-Kutta (sobre el método se 
hablara en la presentacion del proyecto en la FC, si se esta
leyendo este archivo desde el repositorio de GitHub, un resumen 
general se puede apreciar en la siguiente entrada de Wikipedia:
https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods). 

Posteriormente se hacen ejemplos concretos usando casos especiales 
de este, como son el metodo de Euler (R-K de Primer Orden), "el metodo
Runge- Kutta" (R-K de Cuarto Orden), y la regla de los 3/8  (R-K de 
Cuarto Orden con coeficientes del cuadro de Butcher modificados por
Kutta-1901.)
-}

import System.IO


ini :: Double
ini = 0.0

fin :: Double
fin = 10.0

pasos :: Double
pasos = 1000.0

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
       [1/2, 1/2],
       [1/2, 0, 1/2],
       [ 1, 0, 0, 1]]

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

eu :: [[Double]]
eu = [[0, 0],
      [0, 0]]

beuler :: [Double]
beuler = [1]

{--}
kTermRK ::  ((Double, Double)-> Double) -> [[Double]] -> Double -> (Double, Double) -> Int -> Double
kTermRK f but ph (t, x) m
 | m == 1 = f (t, x) 
 | otherwise = f(tStep, xStep)
            where 
                tStep = t + ((but !! (m-1)) !! 0)* ph
                xStep = x + sum (zipWith (*) cns ks) * ph
                      where
                        cns = drop 1 (but !! (m-1))
                        ks = map (kTermRK f but ph (t, x))  [1 .. m-1]

                

pRungeKutta ::  ((Double, Double)-> Double) -> [[Double]] -> [Double] ->  Double -> (Double, Double) -> Int -> (Double, Double)             
pRungeKutta f but b ph (t, x) m = (t', x')
                             where
                                   t' = t + ph
                                   x' = x + ph * sum ( zipWith (*) b (map (kTermRK f but ph (t, x))  [1 .. m]))
                                   





rKutta :: ((Double, Double) -> Double) -> [[Double]] -> [Double] -> Double -> Int -> Double -> Double -> Double -> [(Double, Double)]
rKutta f but b ph m to xo tf = txpar
       where
            iterador = iterate $ (\c -> pRungeKutta f but b ph c m)
            txpar = takeWhile (\( t , x) -> t <= tf ) $ iterador ( to , xo )


func1 :: ( Double , Double ) -> Double
func1 (t, x) = exp x - x*exp t  -- la f (x , y )

rk4data = do
   let rk4f = rKutta func1 rK4 brK4 0.01 4 0 0 5
   outh <- openFile "expx-xexptrk4.csv" WriteMode
   hPrint outh rk4f
   hClose outh

r3d8data = do
   let r3d8f = rKutta func1 r3d8 br3d8 0.01 4 0 0 5
   outh <- openFile "expx-xexptrd8.csv" WriteMode
   hPrint outh r3d8f
   hClose outh

eudata = do
   let euf = rKutta func1 eu beuler 0.01 1 0 0 5
   outh <- openFile "expx-xexpteu.csv" WriteMode
   hPrint outh euf
   hClose outh

func2 :: ( Double , Double ) -> Double
func2 (t, x) = exp 1/t - ((exp 1/t)/t) -- la f (x , y )

rk4data2 = do
   let rk4f = rKutta func2 rK4 brK4 0.01 4 0.393 5 5
   outh <- openFile "xe1dxrk4.csv" WriteMode
   hPrint outh rk4f
   hClose outh

r3d8data2 = do
   let r3d8f = rKutta func2 r3d8 br3d8 0.01 4 0.393 5 5
   outh <- openFile "xe1dxrd8.csv" WriteMode
   hPrint outh r3d8f
   hClose outh

eudata2 = do
   let euf = rKutta func2 eu beuler 0.01 1 0.393 5 5
   outh <- openFile "xe1dxeu.csv" WriteMode
   hPrint outh euf
   hClose outh


func3 :: ( Double , Double ) -> Double
func3 (t, x) = 3*t^2

rk4data3 = do
   let rk4f = rKutta func3 rK4 brK4 0.01 4 0 0 5
   outh <- openFile "x3rk4.csv" WriteMode
   hPrint outh rk4f
   hClose outh

r3d8data3 = do
   let r3d8f = rKutta func3 r3d8 br3d8 0.01 4 0 0 5
   outh <- openFile "x3rd8.csv" WriteMode
   hPrint outh r3d8f
   hClose outh

eudata3 = do
   let euf = rKutta func3 eu beuler 0.01 1 0 0 5
   outh <- openFile "x3eu.csv" WriteMode
   hPrint outh euf
   hClose outh