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


(^+^) :: Vec -> Vec -> Vec
a ^+^ b = Vec (xComp a + xComp b) (yComp a + yComp b) (zComp a + zComp b)

(^-^) :: Vec -> Vec -> Vec
a ^-^ b = Vec (xComp a - xComp b) (yComp a - yComp b) (zComp a - zComp b)

(^*) :: Double -> Vec -> Vec
a ^* b = Vec (a * xComp b) (a * yComp b) (a *zComp b)

(*^) :: Vec -> Double -> Vec
a *^ b = Vec (xComp a * b) (yComp a * b) (zComp a *b)

(^.^) :: Vec -> Vec -> Double
a ^.^ b = (xComp a * xComp b) + (yComp a * yComp b) + (zComp a * zComp b)

pX :: Vec -> Vec -> Vec
pX a b = Vec (yComp a * zComp b - zComp a * yComp b)
             (zComp a * xComp b - xComp a * zComp b) 
             (xComp a * yComp b - yComp a * xComp b)

mag :: Vec -> Double
mag a = sqrt(a ^.^ a)

(^/) :: Double -> Vec -> Vec
a ^/ b = Vec (ai * xComp b) (ai * yComp b) (ai *zComp b)
         where ai = (1/)a

unit  :: Vec -> Vec
unit a = (mag a) ^/ a

vecA :: Vec = Vec 1 2 3


{-Ejercicio 3: Haciendo  uso  de  las  definiciones  para  vectores  que  diste  
en el primer ejercicio calcula la fuerza de Lorentz que siente un electrón -}

lorentz :: Double -> Vec -> Vec -> Vec
lorentz q v b = q ^* (pX v b)

calculo = do
    let qe :: Double = 1.6E-19 --As
    let vecV = Vec 1.1E6 1.2E6 0 --ms^{-1}
    let vecB = Vec 0 1.3E-2 0.1E-2 --N(Am)^{-1}

    let f = lorentz qe vecV vecB
    putStr "Fuerza de Lorentz de un electrón bajo las condiciones dadas (N) = "   
    putStr ("(" ++ show (xComp f) ++ ", ") 
    putStr (show (yComp f) ++ ", ") 
    putStrLn ( show (zComp f) ++ ")") 