--declaración de constantes

{-suponiendo mi centro de gravedad esta en ´
mi centro geometrico (en metros) -}
cjul :: Double = 0.815

-- constante de gravitación universal
gconst :: Double = 6.6743e-11

-- mi masa kg
m :: Double = 85.00

fgrav p r = (gconst * p * m) / ((r + cjul)**2)

mPlanetas = [5.972e24,7.349e22,1.024e26]
rPlanetas = [6.371e6, 1.7374e6, 2.4622e7]

zip' :: [a] -> [b] -> [(a, b)]
zip' xs     []     = []
zip' []     ys     = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

