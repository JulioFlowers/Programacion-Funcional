datos = [4.35, 5.21, 4.72, 4.88, 5.11, 9.23, 11.0, 9.67,10.1, 8.89]

tiempos = take (length datos `div` 2) datos
posiciones = drop(length datos `div` 2) datos

sumPairs :: [Int] -> [Int] 
        sumPairs (x:[]) = [] 
        sumPairs (x1:x2:xs) = (x1 + x2) : sumPairs (x2:xs) 
 