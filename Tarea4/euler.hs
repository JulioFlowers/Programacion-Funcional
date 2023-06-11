
{-
Perez Flores Julio Alfonso TSFC II, Tarea 4

Ejercicio 1: Usa el m ́etodo de Euler para resolver las siguientes ecuaciones diferenciales:

a) dy/dx + 2y = x³exp(-2x), con y(0) = 1.
b) dy/dx + 2y² = xy + x², con y(0) = 1.
c) dy/dx = 1 + 2xy, con y(0) = 3

-}

{-Instrucciones para instalar  el cliente de matplotlib

Ubuntu y Fedora:

Ubuntu: sudo apt-get install -y python3 python3-pip dvipng
Fedora: sudo dnf install -y python3 python3-pip dvipng
Windows: Bajar los binarios de python 

(windows usar: py -m pip  install matplotlib numpy tk basemap scipy)
python3 -m pip install matplotlib numpy tk basemap scipy 

cabal install --lib matplotlib

-}

{--Notas: Se deja el codigo que grafica con Matplotlib expresado, en caso de poder instalarlo
solo hay que descomentar, en caso de no ser posible se adjuntan las graficas corresponientes
las cuales tienen el nombre 

                                    TSFCT4GraphXej1.png

donde X es el inciso de la ecuación
 --}

{-- En caso de haber podido  instalar el cliente de matplotlib descomentar
{-# language ExtendedDefaultRules, ScopedTypeVariables, QuasiQuotes, ParallelListComp #-}

import System.IO
import Graphics.Matplotlib
--}

{- Estas funciones obtiene el primer y segundo elemento de un par, sirve para graficar 
en maplotlib -}
cabeza  :: (a,b) -> a
cabeza (x,_) = x

cola :: (a,b) -> b
cola (_,y) = y

--Las funciones que se nos pide integrar
funca :: ( Double , Double ) -> Double
funca (x , y) = x**3 * exp a  - 2*y
       where a = -2*x

funcb :: ( Double , Double ) -> Double
funcb (x , y) = x*y + x**2 - 2*y**2

funcc :: ( Double , Double ) -> Double
funcc (x , y) = 1 + 2*x*y

-- Aplicacion del metodo de euler
pasoEuler :: (( Double , Double ) -> Double ) -> Double -> ( Double , Double ) -> ( Double , Double )
pasoEuler f ps (x , y) = (x', y')
         where
              y' = y + ps * f (x , y )
              x' = x + ps

-- Obtencion de las eneadas del metodo de euler en un rango establecido por el usuario
euler :: (( Double , Double ) -> Double ) -> Double -> Double -> Double -> Double -> [( Double , Double )]
euler f1 xin xm yin ps = xtpar
     where
          iterador = iterate $ pasoEuler f1 ps
          xtpar = takeWhile (\( x , y) -> x <= xm ) $ iterador ( xin , yin )


data1 = do
  let dat1 = euler funca 0 1 1 0.001
  let dat5 = euler funca 0 5 1 0.01
  let dat10 = euler funca 0 10 1 0.1
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de a) rango: [0,1] h= 0.001" 
  putStrLn (show dat1) 
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de a) rango: [0,5] h= 0.01" 
  putStrLn (show dat5) 
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de a) rango: [0,10] h= 0.1" 
  putStrLn (show dat10) 

{-- En caso de haber podido  instalar el cliente de matplotlib descomentar
data1cg = do
  let dat1 = unzip $ euler funca 0 1 1 0.001
  let dat5 = unzip $ euler funca 0 5 1 0.01
  let dat10 = unzip $ euler funca 0 10 1 0.1

  let x1 = cabeza dat1
  let x5 = cabeza dat5
  let x10 = cabeza dat10

  let y1 = cola dat1
  let y5 = cola dat5
  let y10 = cola dat10

  onscreen $  plot x1 y1 @@ [o2 "label" "Eu R: 0-1 h = 0.001"] %
              plot x5 y5 @@ [o2 "label" "Eu R: 0-5 h = 0.01" , o2 "alpha" 0.4] %
              plot x10 y10 @@ [o2 "label" "Eu R: 0-10 h = 0.1 ", o2 "alpha" 0.4 ]%
              plotMapLinear (\x -> ((x**4 + 4)/(4 * exp (2*x)))) (0) 10 1000 @@ [o2 "label" "analitica", o2 "alpha" 0.5]%
              legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Ecuación a)", o2 "loc" "upper right"]
--}


data2 = do
  let dat1 = euler funcb 0 1 1 0.001
  let dat5 = euler funcb 0 5 1 0.01
  let dat10 = euler funcb 0 10 1 0.1
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de b) rango: [0,1] h= 0.001" 
  putStrLn (show dat1) 
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de b) rango: [0,5] h= 0.01" 
  putStrLn (show dat5) 
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de b) rango: [0,10] h= 0.1" 
  putStrLn (show dat10) 

{-- En caso de haber podido  instalar el cliente de matplotlib descomentar
data2cg = do
  let dat1 = unzip $ euler funcb 0 1 1 0.001
  let dat5 = unzip $ euler funcb 0 5 1 0.01
  let dat10 = unzip $ euler funcb 0 10 1 0.1

  let x1 = cabeza dat1
  let x5 = cabeza dat5
  let x10 = cabeza dat10

  let y1 = cola dat1
  let y5 = cola dat5
  let y10 = cola dat10

  onscreen $  plot x1 y1 @@ [o2 "label" "Eu R: 0-1 h = 0.001"] %
              plot x5 y5 @@ [o2 "label" "Eu R: 0-5 h = 0.01" , o2 "alpha" 0.6] %
              plot x10 y10 @@ [o2 "label" "Eu R: 0-10 h = 0.1 ", o2 "alpha" 0.35 ]%
              legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Ecuación b)", o2 "loc" "upper left"]
--}


data3 = do
  let dat1 = euler funcb 0 1 3 0.001
  let dat2 = euler funcb 0 2 3 0.01
  let dat3 = euler funcb 0 3 3 0.1
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de b) rango: [0,1] h= 0.001" 
  putStrLn (show dat1) 
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de b) rango: [0,2] h= 0.01" 
  putStrLn (show dat2) 
  putStr "Parejas obtenidas por Metodo de Euler para la ecuacion de b) rango: [0,3] h= 0.1" 
  putStrLn (show dat3) 

{-- En caso de haber podido  instalar el cliente de matplotlib descomentar
data3cg = do
  let dat1 = unzip $ euler funcc 0 1 3 0.001
  let dat2= unzip $ euler funcc 0 2 3 0.01
  let dat3 = unzip $ euler funcc 0 3 3 0.1

  let x1 = cabeza dat1
  let x2 = cabeza dat2
  let x3 = cabeza dat3

  let y1 = cola dat1
  let y2 = cola dat2
  let y3 = cola dat3

  onscreen $  plot x1 y1 @@ [o2 "label" "Eu R: 0-1 h = 0.001"] %
              plot x2 y2 @@ [o2 "label" "Eu R: 0-2 h = 0.01", o2 "alpha" 0.5] %
              plot x3 y3 @@ [o2 "label" "Eu R: 0-2 h = 0.01", o2 "alpha" 0.35] %
              legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Ecuación c)", o2 "loc" "upper left"] 
--}