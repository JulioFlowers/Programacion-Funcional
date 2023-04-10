

punto a b | length a == length b = sum (zipWith (*) a b)
          |otherwise = error"Vectores con diferente longitud"

calculo = do  
    let cal =  punto [1,2,3] [2,3,1]
    putStr "<(1,2,3),(2,3,1)> = "   
    putStrLn (show cal) 
    
