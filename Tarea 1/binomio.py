"""
Perez Flores Julio Alfonso TSFC II, Tarea 1
Ejercicio 4: Binomio de Newton.
"""

"""Esta parte corresponde a las filas del triangulo de pascal vista en clase"""
#------------------------------------------------------------------------------
def pascal (n):
     if n ==1:
         return [1]
     else :
          r= pascal (n -1)
          x =[0]+ r
          y=r +[0]
          return [i+j for i,j in zip (x,y)]

def nats (n):
     yield n
     yield from nats (n+1)
#------------------------------------------------------------------------------

"""Definimos una función que eleva los coeficientes numericos del binomio a 
las potencias de 0 a el maximo grado del binomio y las guarda en un array"""
def binnum (n,mxgrd):
     
     bnumcoff = []

     for i in range(mxgrd+1):
         bnumcoff = bnumcoff + [pow(n,i)]

     return bnumcoff

# genera los naturales del 1 al 7
s= nats (7)

#fila correspondiente al triangulo de pascal para el binomio de grado 6
pcoeff = pascal ( next (s))

# elevamos el termino 2 a las diferenes potencias hasta el 6
anum = binnum (2,6)
#las invertimos para multiplicarlas directamente
anum.reverse()

# elevamos el termino 1 a las diferenes potencias hasta el 6
bnum = binnum (1,6)

""" Esta función representa al binomio multipliando los diferentes 
terminos del binomio de la siguiente forma 

a^{0}x^{n} + .... + a^{n}x^{0}

"""

def represent(pc,an, bn, mxgrd):
     
     print("Expancion de (2x+1)^6: ", end='')

     ptt = mxgrd+1
    
     for i in range(ptt): 
          
          pot = mxgrd-i

          if i < mxgrd  :       
              x = "x^" + str(pot)

              print(str(pc[i]*an[i]*bn[i]) + x + " + " ,end='')

          else:       
              print(str(pc[i]*an[i]*bn[i]))


represent(pcoeff,anum,bnum,6)