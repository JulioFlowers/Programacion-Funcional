
mcd :: Int -> Int -> Int
mcd a b | a <= 0 || a <= 0 = error "alguna de las entradas es negativa o 0." 
        | a == b = a
        | a < b = mcd a (b-a)
        | otherwise = mcd (a-b) b