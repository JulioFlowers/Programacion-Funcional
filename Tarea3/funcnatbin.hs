
{-
Perez Flores Julio Alfonso TSFC II, Tarea 3

Ejercicio 5: Define una funci ́on que pase de n ́umeros naturales a binarios 
y viceversaen el orden correcto.
-}

{-Esta función checa si la entrada entera es negativa o cero en ese caso marca error, despues reversa el orden del 
 de la función natToBin la cual es una funcion recursiva, la cual obtine el modulo 2 de n y lo concatena con la funcion 
 nattoBin evaluada en n/2  -}
nat2bin :: Integer -> String
nat2bin n
   | n < 0     = error "La entrada solo admite N U {0}"
   | n == 0    = "0"
   | otherwise = reverse $ natToBin n
                 where natToBin n
                               | n == 0  = ""
                               | otherwise = show (n `mod` 2) ++ natToBin (n `div` 2)

--FUNCIONES AUXILIARES: Para convertir de binario a decimal

{-Convierte el string a una lista con elementos de tipo Integer, usando read as Integer-}
strnum :: String  -> [Integer]
strnum s= map (read . (:"")) s

{- Multiplica cada elemento  de la lista por 2 -}
pertwo :: Num a => [a] -> [a]
pertwo xs = [ x*2 | x<-xs]

{-Es la función de los apuntes del recursión que te da la longitud del arreglo-}
tam :: Num a => [t] -> a
tam xs = sum [1| _<- xs]

{- Esta función convierte el numero binario que se proporciono como un string en un 
array de numero enteros, lo invierte lo multiplica por 2 y despues eleva cada elemento 
de la lista a su respectivo indice usando otro array de numeros enteros que va del 0 al 
tamaño de la lista - 1, y la función zipWith con la potencia-}

bin2nat :: String -> Integer
bin2nat s = sum  $ zipWith (^) arrbin expons
            where arrbin = pertwo $ reverse $ strnum s
                  expons = [0 .. (tam arrbin) -1]







