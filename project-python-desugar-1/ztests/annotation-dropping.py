def f(x: int) -> int: pass

def y(x:int,*,y:int,z,w:int):
    return x + 1


def y(x:int,*d:int,y:int,z,w:int,**r:int):
    return x + 1

