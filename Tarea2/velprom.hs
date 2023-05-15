
{-
Perez Flores Julio Alfonso TSFC II, Tarea 2
Ejercicio 5: Velocidades promedio.
-}

--datos proporcioados por el ejercicio
datos = [4.35, 5.21, 4.72, 4.88, 5.11, 9.23, 11.0, 9.67,10.1, 8.89]

{-Usamos la definición de la función ordenar dada en los apuntes
y con el operador filer para ordenar de menos a mayor valor una 
lista -}
ordenar [] = []
ordenar ( x : xs ) = ordenar chico ++ [ x ] ++ ordenar grande
      where
         chico = filter (< x) xs
         grande = filter (>= x) xs


{-Como el tiempo siempre es una variable definida positiva y creciente 
obtenemos los tiempos haciendo una lista que tome los elementos de 
la lista datos, hasta que la longitud de la nueva lista sea 6, y despues
le pedimos que la ordene.-}
tiempos = ordenar (take (length datos `div` 2) datos)

{-Toma los 6 elementos de la lista de datos cuya longitud sea mayor a 6 
y los denominamos como las posiciones-}
posiciones = drop(length datos `div` 2) datos


{-Construimos una funcion para poder calcular la resta de un termino de la 
lista con su inmediato anterior para poder definir Delta T y Delta V, esto lo
hacemos quitando los terminos de los extremos que no pueden completar el par 
anterior (headless) y posterior (tailess) y restamos esas listas, generando 
una nueva lista con esas diferencias.-}

diferencia a = zipWith (-) headless tailess 
     where 
        headless = drop 1 a
        tailess = take(length a - 1) a

-- defincion de la velocidad como función curriada
velocidad dx dt = dx / dt 


{-No entendi si nos pedian las velocidad promedio de los puntos, o el promedio
de las velocidades de los puntos, asi que tambien construi una función para 
sacar el promedio por si las dudas.-}
prom b 
    | not (null b) = sum b / fromIntegral(length b) :: Double
    | otherwise = error"No hay elementos"

{-
Esta variable le dice al interprete que haga una serie de instrucciones
primero que cree una nueva lista cuyos elementos son las velocidades promedio
de los 6 puntos creando las Delta T y Delta X que usa la funcion velocidad
con la funcion diferencia, la cual se aplica a las listas posiciones y tiempos

Posterimente le pedimos que imprima cada elemento del arreglo con un 
texto descriptivo a la par
-}

datavel = do  
    let cal = zipWith velocidad (diferencia posiciones) (diferencia tiempos)
    putStr "Velocidades promedio para los 6 puntos (ms⁻¹): "   
    putStrLn (show cal) 