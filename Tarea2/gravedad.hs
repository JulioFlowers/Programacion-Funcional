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

pesos = do  
    let pcal = zipWith fgrav mPlanetas rPlanetas
    putStr "Mi peso en la Tierra: "   
    putStrLn (show (pcal !! 0) ++ " N") 
    putStr "Mi peso en la Luna: "   
    putStrLn (show (pcal !! 1)  ++ " N")
    putStr "Mi peso en Neptuno: "   
    putStrLn (show (pcal !! 2)  ++ " N")