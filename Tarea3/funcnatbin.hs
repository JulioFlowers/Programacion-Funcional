
nat2bin :: Integer -> String
nat2bin n
   | n < 0     = error "La entrada solo admite N U {0}"
   | n == 0    = "0"
   | otherwise = reverse $ natToBin n
                 where natToBin n
                               | n == 0  = ""
                               | otherwise = show (n `mod` 2) ++ natToBin (n `div` 2)


   
