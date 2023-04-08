def fact (x): # definimos la funcion fact
 if x ==0: # caso base
     return 1
 else :
     return x* fact (x -1) # para los demas casos

x = int ( input (" Valor a calcular factorial :") ) #dar valor
print ( fact (x)) # imprime el resultado