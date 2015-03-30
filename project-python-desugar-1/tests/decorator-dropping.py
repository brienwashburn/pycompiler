@a
def f(): pass

@a.b
def f(): pass

@a.b(x)
def f(): pass

@a.b(x)
@c
def f(): pass

@a.b(x,*y,z,**w)
@c
def f(): pass


@a.b(x,*y,z,**w)
@c
@d()
def f(): pass
