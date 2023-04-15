
{-
Perez Flores Julio Alfonso TSFC II, Tarea 2
Ejercicio 2: definición de producto punto y calculo de un producto punto.
-}

{-Defincion de producto punto: Si los vectores tienen la misma cantidad
de elementos entonces defincimos al producto punto co la suma del producto
de los pares ordenados (a_i, b_i) (todo el producto de los pares ordenados
se define con zipWith...), donde a_i es la iesima componente del
vecto A, y y b_i la iesima componente delvecto B

de lo contrario retorna un error-}
punto a b | length a == length b = sum (zipWith (*) a b)
          |otherwise = error"Vectores con diferente longitud"


{-
Esta variable le dice al interprete que haga una serie de instrucciones
primero que evalue en el producto punto los 2 vectores proporcionados para 
el ejercicio.

Posterimente le pedimos que imprima la lista con un 
texto descriptivo a la par
-}
calculo = do  
    let cal =  punto [1,2,3] [2,3,1]
    putStr "<(1,2,3),(2,3,1)> = "   
    putStrLn (show cal) 


---------------------------------------------------------------------------------------------
-- Ejercicio 3: definición de producto cruz y calculo del volumen de paralelepipedo.

{-Defincion de producto cruz: Si los vectores tienen la misma cantidad
de elementos y son de 3 dimensiones, entonces defincimos al producto cruz usando 
la definición matematiCA Y obteniendo la componente del vector haciendo uso del 
indice de la componente en la lista.

de lo contrario retorna un error-}

pcruz a b
  | length a == 3 && length b == 3 =
      [ a !! 1 * b !! 2 - a !! 2 * b !! 1,
        a !! 2 * b !! 0 - a !! 0 * b !! 2,
        a !! 0 * b !! 1 - a !! 1 * b !! 0
      ]
  | otherwise = error "A o B no es un vector tridimensional"

{-
Esta variable le dice al interprete que haga una serie de instrucciones
primero que evalue en el producto punto del primer vector proporcionado con el producto
cruz de los otros 2 vectores proporcionados.

Posterimente le pedimos que imprima la lista con un 
texto descriptivo a la par
-}
volumen = do
  let cal = punto [1, 2, 3] (pcruz [2, 3, 1] [3,5,1])
  putStr "Area del paralelepipedo generado por (1, 2, 3); (2, 3, 1); (3,5,1) = "
  putStrLn (show cal)


---------------------------------------------------------------------------------------------
-- Ejercicio 3:  calculo de la Torca

{-
Esta variable le dice al interprete que haga una serie de instrucciones
primero que evalue en el producto cruz los 2 vectores proporcionados para 
el ejercicio.

Posterimente le pedimos que imprima la lista con un 
texto descriptivo a la par
-}
torca = do  
    let cal =  pcruz  [0,0,5] [10,5,0]
    putStr "Torca (rxF) = "   
    putStrLn (show cal) 