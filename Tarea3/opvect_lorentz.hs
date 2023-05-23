import Control.Monad.Trans.RWS (put)
{-
Perez Flores Julio Alfonso TSFC II, Tarea 3

Ejercicio 1: Define las operaciones para el data de vectores como vimos en clase para haskell:

-Suma, resta vectorial

-Producto (por izquierda y derecha)

-división escalar

-Producto punto y producto cruz vectorial

-Norma de un vector
-}

-- Definción de la estructura de Vector.
data Vec = Vec { xComp :: Double,
                 yComp :: Double,
                 zComp :: Double} deriving Show

--SUMA VEC: Se suma componente a componente de 2 estructuras de datos Vector.
(^+^) :: Vec -> Vec -> Vec
a ^+^ b = Vec (xComp a + xComp b) (yComp a + yComp b) (zComp a + zComp b)

--RESTA VEC: Se resta componente a compoyente de 2 estructuras de datos Vector.
(^-^) :: Vec -> Vec -> Vec
a ^-^ b = Vec (xComp a - xComp b) (yComp a - yComp b) (zComp a - zComp b)

{- PRODUCTO ESCALAR IZQ: Se multiplica el escalar por cada componente del vector, por la izquierda -}
(^*) :: Double -> Vec -> Vec
a ^* b = Vec (a * xComp b) (a * yComp b) (a *zComp b)

{-PRODUCTO ESCALAR DER: Se multiplica el escalar por cada componente del vector, por la derecha-}
(*^) :: Vec -> Double -> Vec
a *^ b = Vec (xComp a * b) (yComp a * b) (zComp a *b)

{-PRODUCTO VEC: Se multiplica componente a componente de 2 estructuras de datos Vector y se suman-}
(^.^) :: Vec -> Vec -> Double
a ^.^ b = (xComp a * xComp b) + (yComp a * yComp b) + (zComp a * zComp b)

{-PRODUCTO CRUZ: Se define como la regla nemotecnica-}
pX :: Vec -> Vec -> Vec
pX a b = Vec (yComp a * zComp b - zComp a * yComp b)
             (zComp a * xComp b - xComp a * zComp b) 
             (xComp a * yComp b - yComp a * xComp b)

{-NORMA: Define como la raiz cuadrada del producto punto -}
mag :: Vec -> Double
mag a = sqrt(a ^.^ a)

{-Aqui no entendi que especificaba el profe asi que hice 2 operaciones. La división por un escalar 
que realmente es una multiplicación escalar por la izq usando el inverso del numero proporcionado-}
(^/) :: Double -> Vec -> Vec
a ^/ b = Vec (ai * xComp b) (ai * yComp b) (ai *zComp b)
         where ai = (1/)a

--La segunda es la normalización de un vector 
unit  :: Vec -> Vec
unit a = (mag a) ^/ a

vecA = Vec 1 0 0
vecB = Vec 0 1 0

--prueba de las funciones
prueba = do
         putStrLn ("VecA: " ++ show(vecA) ++ ", Vec B: " ++ show(vecB)) 
         putStrLn ("Suma: " ++ show( vecA^+^vecB )) 
         putStrLn ("Resta: " ++ show( vecA^-^vecB )) 
         putStrLn ("Producto esc. por la izq (2*A): " ++ show( 2^*vecA ))
         putStrLn ("Producto esc. por la der (A*2): " ++ show( vecA*^2 ))
         putStrLn ("Podructo punto: " ++ show( vecA^.^vecB )) 
         putStrLn ("Podructo cruz: " ++ show( pX vecA vecB ))
         putStrLn ("Norma Vector A: " ++ show (mag vecA))
         putStrLn ("división de vecA entre 2: " ++ show ( 2 ^/ vecA))
         putStrLn ("Normalización del vector A + B: " ++ show (unit (vecA ^+^ vecB)))


{-Ejercicio 3: Haciendo  uso  de  las  definiciones  para  vectores  que  diste  
en el primer ejercicio calcula la fuerza de Lorentz que siente un electrón -}

{-Se define una funcion con la formula de la fuerza de lorentz usando las funciones
multiplicación escalar por la izquierda y el producto cruz -}
lorentz :: Double -> Vec -> Vec -> Vec
lorentz q v b = q ^* (pX v b)

calculo = do
    --Se definen las cantidades vectorial con las unidades pertinentes
    let qe :: Double = 1.6E-19 --As
    let vecV = Vec 1.1E6 1.2E6 0 --ms^{-1}
    let vecB = Vec 0 1.3E-2 0.1E-2 --N(Am)^{-1}

    let f = lorentz qe vecV vecB
    putStr "Fuerza de Lorentz de un electrón bajo las condiciones dadas (N) = "   
    putStr ("(" ++ show (xComp f) ++ ", ") 
    putStr (show (yComp f) ++ ", ") 
    putStrLn ( show (zComp f) ++ ")") 