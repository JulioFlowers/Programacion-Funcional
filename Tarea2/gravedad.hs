{-
Perez Flores Julio Alfonso TSFC II, Tarea 2
Ejercicio 1: Función Curriada Ley de Gravitación Universal.
-}

--declaración de constantes

{-suponiendo mi centro de gravedad esta en ´
mi centro geometrico (en metros) -}
cjul :: Double = 0.815

-- constante de gravitación universal
gconst :: Double = 6.6743e-11

-- mi masa kg
m :: Double = 85.00

{- Definición de la ley de la gravitación como una función curriada-}
fgrav p r = (gconst * p * m) / ((r + cjul)**2)

-- masa de la Tierra, Luna y Neptuno
mPlanetas = [5.972e24,7.349e22,1.024e26]

-- radio de la Tierra, Luna y Neptuno
rPlanetas = [6.371e6, 1.7374e6, 2.4622e7]

{-
Esta variable le dice al interprete que haga una serie de instrucciones
primero que cree una nueva lista cuyos elementos son F(M_i, r_i) con
i perteneciente a {1,2,3} donde M_i es elemento de la lista de mPlanetas
y r_i elemento de rPlanetas.

Posterimente le pedimos que imprima cada elemento del arreglo con un 
texto descriptivo a la par
-}

pesos = do  
    let pcal = zipWith fgrav mPlanetas rPlanetas
    putStr "Mi peso en la Tierra: "   
    putStrLn (show (pcal !! 0) ++ " N") 
    putStr "Mi peso en la Luna: "   
    putStrLn (show (pcal !! 1)  ++ " N")
    putStr "Mi peso en Neptuno: "   
    putStrLn (show (pcal !! 2)  ++ " N")