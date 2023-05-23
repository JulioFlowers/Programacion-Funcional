
{-
Perez Flores Julio Alfonso TSFC II, Tarea 3

Ejercicio2: Implementa el algoritmo de Euclides como una función recursiva. 
Para dos números enteros positivos calcula el máximo com ́un divisor.
-}

{- Esta funcion lo que hace es checar si alguno de los numeros que introduciste 
no es un numero entero positivo, en caso de que sea asi arroja error.

Posteriormente checa si los numeros son iguales, si es ese caso el MCD es el mismo
numer0, si uno es mas grande que el otro se llama de forma recursiva a la función 
MCD con el numero mas chico y la resta del numero mas grande menos el mas chico.-}
mcd :: Int -> Int -> Int
mcd a b | a <= 0 || a <= 0 = error "alguna de las entradas es negativa o 0." 
        | a == b = a
        | a < b = mcd a (b-a)
        | otherwise = mcd (a-b) b