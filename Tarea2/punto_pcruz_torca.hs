punto a b | length a == length b = sum (zipWith (*) a b)
          |otherwise = error"Vectores con diferente longitud"

pcruz a b
  | length a == 3 && length b == 3 =
      [ a !! 1 * b !! 2 - a !! 2 * b !! 1,
        a !! 2 * b !! 0 - a !! 0 * b !! 2,
        a !! 0 * b !! 1 - a !! 1 * b !! 0
      ]
  | otherwise = error "A o B no es un vector tridimensional"

calculo = do  
    let cal =  punto [1,2,3] [2,3,1]
    putStr "<(1,2,3),(2,3,1)> = "   
    putStrLn (show cal) 

volumen = do
  let cal = punto [1, 2, 3] (pcruz [2, 3, 1] [3,5,1])
  putStr "Area del paralelepipedo generado por (1, 2, 3); (2, 3, 1); (3,5,1) = "
  putStrLn (show cal)

torca = do  
    let cal =  pcruz  [0,0,5] [10,5,0]
    putStr "Torca (rxF) = "   
    putStrLn (show cal) 