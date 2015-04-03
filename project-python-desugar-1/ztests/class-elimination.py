class X(y):
    y = 10
    def methods(self):
	return (self.methodA(), self.methodB())

x = X()

print(x.y)

def f(x) -> int:
    return 1
