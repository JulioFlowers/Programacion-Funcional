
{-
Perez Flores Julio Alfonso TSFC II, Tarea 3

Ejercicio 4: Corriente que pasa por una varilla de cobre de 20 metros de longitud y 5 cm de sección 
transversal voltaje de 100volts y resitividad del cobre de 1,71×10−8Ωm
-}

--definimos la intensidad de corriente usando ley de ohm
leyohmI :: Double -> Double -> Double -> Double -> Double
leyohmI v r l s = v / (res r l s) 

{-Definimos la función que calcula la resistencia de los materiales segun sus 
condiciones de longitud, sección transversal y resistividad -}
res :: Double -> Double -> Double -> Double
res r l s = r * l / s

calculo = do 
    {-Calculamos la intrnsidad de corriente introducciendo como argumentos de la función
    leyohmI al voltaje y al resultado de llamar a nuestra función que calcula la resistencia
    a partir de la longitud, sección transversal y resistividad-}
    let cal = leyohmI 100  1.71E-8 20 0.001963

    --lo mostramos en la Terminal
    putStr "I max a 100V (cobre l: 20m S: 0.05m  T: 20-25°C): "  
    putStrLn (show (cal) ++ " A")