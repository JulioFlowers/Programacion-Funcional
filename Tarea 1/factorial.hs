{-
Perez Flores Julio Alfonso TSFC II, Tarea 1
Ejercicio 1: Función Factorial.
-}

factorial :: Integral n => n -> n
factorial 0 = 1
factorial x = x*factorial(x + (-1))

--Ejercicio 2: Función combinatoria.

combinatoria :: Integral n => n -> n -> n
combinatoria x y =  factorial(x) `div` ( factorial( x + (-y)) * factorial(y) )
