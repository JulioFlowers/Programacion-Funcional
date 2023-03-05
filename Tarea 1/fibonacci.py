def fibonacci(n):

    if n <= 1:
        return [n]
    
    else:
        r = fibonacci(n-2) + fibonacci(n-1)
        x = [] + r 
        x = x + [x[len(x)-1] + x[len(x)-2]]
        return x
    
def nats(n):
    yield n
    yield from nats(n + 1)


print ( fibonacci(4))
