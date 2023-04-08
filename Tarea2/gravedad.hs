--declaración de constantes

{-suponiendo mi centro de gravedad esta en ´
mi centro geometrico (en metros) -}
cjul :: Double = 0.815

-- constante de gravitación universal
gconst :: Double = 6.6743e-11

-- mi masa kg
m :: Double = 85.00

fgrav  :: Double -> Double -> Double
fgrav p r = (gconst * p * m) / ((r + cjul)**2)


--let mPlanetas = [5.972e24,7.349e22,1.024e26]
--let raPlanetas = [45,78,78]