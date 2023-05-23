
{-
Perez Flores Julio Alfonso TSFC II, Tarea 3

Ejercicio 5: Define una funci ́on que pase de n ́umeros naturales a binarios 
y viceversaen el orden correcto.
-}

nat2bin :: Integer -> String
nat2bin n
   | n < 0     = error "La entrada solo admite N U {0}"
   | n == 0    = "0"
   | otherwise = reverse $ natToBin n
                 where natToBin n
                               | n == 0  = ""
                               | otherwise = show (n `mod` 2) ++ natToBin (n `div` 2)


bin2nat :: String -> Integer
bin2nat s = sum  $ zipWith (^) arrbin expons
            where arrbin = pertwo $ reverse $ strnum s
                  expons = [0 .. (tam arrbin) -1]



strnum :: String  -> [Integer]
strnum s= map (read . (:"")) s


pertwo :: Num a => [a] -> [a]
pertwo xs = [ x*2 | x<-xs]

tam :: Num a => [t] -> a
tam xs = sum [1| _ <- xs ]



