{-
Perez Flores Julio Alfonso TSFC II, Tarea 4

Ejercicio 2: Usa el método del trapecio y el punto medio para resolver las integrales:

Trata de comparar con resultados analíticos o de otros métodos (es decir, checa con las
tablas) ¿qué tanto es el error? ¿qué puedes hacer parareducirlo?
-}

--Defincion de funciones 
type FuncR = Double -> Double

funca :: FuncR
funca x = x^2

funcb :: FuncR
funcb x = x * exp x

funcc :: FuncR
funcc x = x^2 * cos x

{-calcula el error relativo de la integral numerica
con respecto a la integral analitica -}
errel :: Double -> Double -> Double 
errel ia ni = abs (ia - ni) / ia * 100

--REGLA DEL PUNTO MEDIO
pMint :: FuncR -> Double -> Double -> Double -> Double
pMint f a b dt = 
    sum [f t * dt | t <-[a + dt/2, a + 3*dt/2 .. b - dt/2]]

--REGLA DEL TRAPECIO 
tintg :: FuncR -> Double -> Double -> Double -> Double -> Double -> Double
tintg f x n lim0 newlim0 lim1
      |newlim0 >= lim1 = (x * delta)
      |x == 0 = tintg f (x + (f lim0 + f lim1)/2) n lim0 (newlim0 + delta) lim1
      |otherwise = tintg f (x + f newlim0) n lim0 (newlim0 + delta) lim1
      where delta = ((lim1-lim0)/n)
--inciso a)
-- Integracion analitica Analitica = 1/3

incisoa = do

    --integracion por punto medio con un paso de 0.5
    let ipM05 = pMint funca 0 1 0.5
    --calculo del error
    let ipM05er = errel (1/3) ipM05
    putStrLn ("Inregracion x² de 0 al 1 con dt = 0.5 y metodo del punto medio: " ++ show(ipM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1/3): " ++ show(ipM05er) ++ " %")


    --integracion por punto medio con un paso de 0.1
    let ipM01 = pMint funca 0 1 0.1
    --calculo del error
    let ipM01er = errel (1/3) ipM01
    putStrLn ("Inregracion x² de 0 al 1 con dt = 0.1 y metodo del punto medio: " ++ show(ipM01))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1/3): " ++ show(ipM01er) ++ " %")

     --integracion por punto medio con un paso de 0.01
    let ipM001 = pMint funca 0 1 0.01
    --calculo del error
    let ipM001er = errel (1/3) ipM001
    putStrLn ("Inregracion x² de 0 al 1 con dt = 0.01 y metodo del punto medio: " ++ show(ipM001))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1/3): " ++ show(ipM001er) ++ " %")

    putStrLn ""
    putStrLn ""

    --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funca 0 10 0 0 1
    --calculo del error
    let tintgM05er = errel (1/3) tintgM05
    putStrLn ("Inregracion x² de 0 al 1 haciendo 10 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1/3): " ++ show(tintgM05er) ++ " %")

    --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funca 0 100 0 0 1
    --calculo del error
    let tintgM05er = errel (1/3) tintgM05
    putStrLn ("Inregracion x² de 0 al 1 haciendo 100 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1/3): " ++ show(tintgM05er) ++ " %")

    --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funca 0 1000 0 0 1
    --calculo del error
    let tintgM05er = errel (1/3) tintgM05
    putStrLn ("Inregracion x² de 0 al 1 haciendo 1000 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1/3): " ++ show(tintgM05er) ++ " %")




--inciso b)
-- Integracion analitica Analitica = 1

incisob = do

    --integracion por punto medio con un paso de 0.5
    let ipM01 = pMint funcb 0 1 0.5
    --calculo del error
    let ipM01er = errel (1) ipM01
    putStrLn ("Inregracion xexp(x) de 0 al 1 con dt = 0.5 y metodo del punto medio: " ++ show(ipM01))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1): " ++ show(ipM01er) ++ " %")


    --integracion por punto medio con un paso de 0.1
    let ipM01 = pMint funcb 0 1 0.1
    --calculo del error
    let ipM01er = errel (1) ipM01
    putStrLn ("Inregracion xexp(x) de 0 al 1 con dt = 0.1 y metodo del punto medio: " ++ show(ipM01))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1): " ++ show(ipM01er) ++ " %")

     --integracion por punto medio con un paso de 0.01
    let ipM001 = pMint funcb 0 1 0.01
    --calculo del error
    let ipM001er = errel (1) ipM001
    putStrLn ("Inregracion xexp(x) de 0 al 1 con dt = 0.01 y metodo del punto medio: " ++ show(ipM001))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1): " ++ show(ipM001er) ++ " %")

    putStrLn ""
    putStrLn ""

     --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funcb 0 10 0 0 1
    --calculo del error
    let tintgM05er = errel 1 tintgM05
    putStrLn ("Inregracion xexp(x) de 0 al 1 haciendo 10 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1): " ++ show(tintgM05er) ++ " %")

    --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funcb 0 100 0 0 1
    --calculo del error
    let tintgM05er = errel 1 tintgM05
    putStrLn ("Inregracion xexp(x) de 0 al 1 haciendo 100 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1/3): " ++ show(tintgM05er) ++ " %")

    --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funcb 0 1000 0 0 1
    --calculo del error
    let tintgM05er = errel 1 tintgM05
    putStrLn ("Inregracion xexp(x) de 0 al 1 haciendo 1000 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (1): " ++ show(tintgM05er) ++ " %")




--inciso c)
-- Integracion analitica Analitica = pi²/4 - 2 aprox(0.46740)

incisoc = do

    --integracion por punto medio con un paso de 0.5
    let ipM01 = pMint funcc 0 (pi/2) 0.5
    --calculo del error
    let ipM01er = errel (pi**2 / 4 - 2) ipM01
    putStrLn ("Inregracion x²cos(x) de 0 al pi/2 con dt = 0.5 y metodo del punto medio: " ++ show(ipM01))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (pi²/4 - 2 aprox(0.46740)): " ++ show(ipM01er) ++ " %")


    --integracion por punto medio con un paso de 0.1
    let ipM01 = pMint funcc 0 (pi/2) 0.1
    --calculo del error
    let ipM01er = errel (pi**2 / 4 - 2) ipM01
    putStrLn ("Inregracion  x²cos(x) de 0 al pi/2 con dt = 0.1 y metodo del punto medio: " ++ show(ipM01))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (pi²/4 - 2 aprox(0.46740)): " ++ show(ipM01er) ++ " %")

     --integracion por punto medio con un paso de 0.01
    let ipM001 = pMint funcc 0 (pi/2) 0.01
    --calculo del error
    let ipM001er = errel (pi**2 / 4 - 2) ipM001
    putStrLn ("Inregracion  x²cos(x) de 0 al pi/2 con dt = 0.01 y metodo del punto medio: " ++ show(ipM001))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (pi²/4 - 2 aprox(0.46740)): " ++ show(ipM001er) ++ " %")

    putStrLn ""
    putStrLn ""

     --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funcc 0 10 0 0 (pi/2)
    --calculo del error
    let tintgM05er = errel (pi**2 / 4 - 2) tintgM05
    putStrLn ("Inregracion x²cos(x) de 0 al (pi/2) haciendo 10 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (pi²/4 - 2 aprox(0.46740)): " ++ show(tintgM05er) ++ " %")

    --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funcc 0 100 0 0 (pi/2)
    --calculo del error
    let tintgM05er = errel (pi**2 / 4 - 2) tintgM05
    putStrLn ("Inregracion x²cos(x) de 0 al (pi/2) haciendo 100 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (pi²/4 - 2 aprox(0.46740)): " ++ show(tintgM05er) ++ " %")

    --integracion por trapecio con 10 particiones
    let tintgM05 = tintg funcc 0 1000 0 0 (pi/2)
    --calculo del error
    let tintgM05er = errel (pi**2 / 4 - 2) tintgM05
    putStrLn ("Inregracion x²cos(x) de 0 al (pi/2) haciendo 1000 particiones con el metodo trapecio: " ++ show(tintgM05))
    putStrLn ("Error relativo comparado con el valor de la integración analitica (pi²/4 - 2 aprox(0.46740)): " ++ show(tintgM05er) ++ " %")