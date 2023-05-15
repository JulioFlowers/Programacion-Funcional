{-
Perez Flores Julio Alfonso TSFC II, Tarea 1
Ejercicio 1: Función Factorial.
-}

{-
  Definimos una funcion de tipo integral y 
  de una sola variable a una sola variable donde 
  para el = el factorial es 1 y para los demas 
  casos el factorial es el producto de x con el 
  facortial de x-1
-}
factorial :: Integral n => n -> n
factorial 0 = 1
factorial x = x*factorial(x + (-1))

--Ejercicio 2: Función combinatoria.
combinatoria :: Integral n => n -> n -> n
combinatoria x y =  factorial(x) `div` ( factorial( x + (-y)) * factorial(y) )
