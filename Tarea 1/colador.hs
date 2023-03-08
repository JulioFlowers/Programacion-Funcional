{-
Perez Flores Julio Alfonso TSFC II, Tarea 1
Ejercicio 5: Colador de Eratóstenes.
-}

-- declaramos el tipo de datos
primos :: [Integer]

{- Declaramos la variable primos como el llamado de 
la función colar a la lista infinita de enteros
mayores a 2. -}
primos = colar[2..]

{- Definimos la funcion colar con una lista de al menos 
un elemento p al cual ya le daremos el estatus de numero 
primo, y a los demas elementos de la lista verificamos 
que el modulo entre el elemento p y el elemento de la lista
sea mayor que 0, es decir que x no sea multiplo de p.-}
colar(p:ps) = p : colar[x | x <- ps, x `mod` p > 0]