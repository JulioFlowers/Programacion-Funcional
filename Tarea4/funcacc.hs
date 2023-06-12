{-
Perez Flores Julio Alfonso TSFC II, Tarea 4

Ejercicio 3:
Similar al caso del satélite y el oscilador forzado-amortiguado, implementala función 
de aceleración que es repelido por una carga estática de la misma magnitud  y  signo.
Evalúa  el  sistema  dinámico  y  da  algunos  valores  del resultado.
-}


-- Definción de la estructura de Vector.
data Vec = Vec { xComp :: Double,
                 yComp :: Double,
                 zComp :: Double} deriving Show

type Temp = Double
type Desp = Vec
type Vel = Vec
type Est = ( Temp , Desp , Vel )

type FunAcc =  Est -> Vec

--SUMA VEC: Se suma componente a componente de 2 estructuras de datos Vector.
(^+^) :: Vec -> Vec -> Vec
a ^+^ b = Vec (xComp a + xComp b) (yComp a + yComp b) (zComp a + zComp b)

--RESTA VEC: Se resta componente a compoyente de 2 estructuras de datos Vector.
(^-^) :: Vec -> Vec -> Vec
a ^-^ b = Vec (xComp a - xComp b) (yComp a - yComp b) (zComp a - zComp b)

{- PRODUCTO ESCALAR IZQ: Se multiplica el escalar por cada componente del vector, por la izquierda -}
(^*) :: Double -> Vec -> Vec
a ^* b = Vec (a * xComp b) (a * yComp b) (a *zComp b)

{-PRODUCTO ESCALAR DER: Se multiplica el escalar por cada componente del vector, por la derecha-}
(*^) :: Vec -> Double -> Vec
a *^ b = Vec (xComp a * b) (yComp a * b) (zComp a *b)

{-PRODUCTO VEC: Se multiplica componente a componente de 2 estructuras de datos Vector y se suman-}
(^.^) :: Vec -> Vec -> Double
a ^.^ b = (xComp a * xComp b) + (yComp a * yComp b) + (zComp a * zComp b)

{-PRODUCTO CRUZ: Se define como la regla nemotecnica-}
pX :: Vec -> Vec -> Vec
pX a b = Vec (yComp a * zComp b - zComp a * yComp b)
             (zComp a * xComp b - xComp a * zComp b) 
             (xComp a * yComp b - yComp a * xComp b)

{-NORMA: Define como la raiz cuadrada del producto punto -}
mag :: Vec -> Double
mag a = sqrt(a ^.^ a)

{-Aqui no entendi que especificaba el profe asi que hice 2 operaciones. La división por un escalar 
que realmente es una multiplicación escalar por la izq usando el inverso del numero proporcionado-}
(^/) :: Double -> Vec -> Vec
a ^/ b = Vec (ai * xComp b) (ai * yComp b) (ai *zComp b)
         where ai = (1/)a

--La segunda es la normalización de un vector 
unit  :: Vec -> Vec
unit a = (mag a) ^/ a

{-Tenemos la sig. expresión

                     a = F_e / m, 

 como solo tenemos 2 pariculas con carga interactuando
 tenemos de la ley de Coulomb:

                  a = (K/m) (2q / |r²|) r

para mayor facilidad supongamos que estamos en el vacio

                K  = 1 / 4*pi*e_o 
-}

--Definicon de la función
accRep :: FunAcc 
accRep (t, r, v) = (k * (2*q/mag r ^2)) ^* u
      where
           k= 1 / 4*pi*ep_0*m
           ep_0 = 8.8549E-12 --[C²N⁻¹m⁻²]
           q = -1.6E-19 --[C] carga electrón
           m = 9.1E-31 --[kg] masa electrón
           u = unit r -- vector unitario r



-- Aplicacion del metodo de euler
pasoEuler :: FunAcc -> Double -> Est -> Est
pasoEuler a dt (t , r , v) = (t',r' ,v')
    where
         t' = t + dt
         r' = r ^+^ v *^ dt
         v' = v ^+^ a (t ,r ,v) *^ dt

-- Esta funcion itera sobre el la funcion pasoEuler
euler :: FunAcc -> Double -> Est -> [ Est ]
euler a dt = iterate ( pasoEuler a dt )

{-Condiciones Iniciales

  posición: Reulizamos las mismas del satelite,

  velocidad: Partimos del reposo.
-}
rr :: Vec
rr = Vec 1.0 (-2.1) 0.5

vv :: Vec
vv = Vec 0 0 0


calculo = do
   --Le pedimos a la iteracion 10 estados 
   let estados = take 10 $ euler accRep 0.01 (1.0 , rr , vv )
   putStrLn (show estados) 
