"""
Perez Flores Julio Alfonso TSFC II, Tarea 1
Ejercicio 3: Sucesión de Fibonacci.
"""

""" Se define una función donde los pasos base son f_{0} = 0 
y f_{1} = 1, si x no es 0 ni 1 entonces el termino se define 
como la suma de f_{n-2} + f_{n-1}
"""
def fibonacci(n):

    if n <= 1:
        return n
    
    else:

        for i in range(n):
              r = fibonacci(n-2) + fibonacci(n-1)
              
        return r

"""Esta función guarda todos los n terminos de la sucesión de fibonacci en un array 
y los  imprime."""
def printfib(n):
    succ = []

    for i in range(n+1):
        succ = succ + [fibonacci(i)]
    
    return succ

#generador flojo de naturales
def nats(n):
    yield n
    yield from nats(n + 1)


x = int ( input (" Sucesión de fibonacci hasta el termino: ") )
s = nats(x)

print (printfib(next(s)))