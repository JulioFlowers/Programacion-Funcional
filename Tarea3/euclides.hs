
{-
Perez Flores Julio Alfonso TSFC II, Tarea 3

Ejercicio2: Implementa el algoritmo de Euclides como una función recursiva. 
Para dos números enteros positivos calcula el máximo com ́un divisor.
-}

mcd :: Int -> Int -> Int
mcd a b | a <= 0 || a <= 0 = error "alguna de las entradas es negativa o 0." 
        | a == b = a
        | a < b = mcd a (b-a)
        | otherwise = mcd (a-b) b