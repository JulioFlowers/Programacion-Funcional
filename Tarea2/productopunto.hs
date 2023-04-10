

punto a b | length a == length b = sum (zipWith (*) a b)
          |otherwise = error"Vectores con diferente longitud"