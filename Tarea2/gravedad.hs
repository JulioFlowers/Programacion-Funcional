--declaración de constantes

{-suponiendo mi centro de gravedad esta en ´
mi centro geometrico (en metros) -}
cjul  = 0.815

-- constante de gravitación universal
let g :: Fractional Double = 0.00000000006674 

-- mi masa kg
m  = 85

fgrav p r = (g*p*m) `div` (r + cjul)**2