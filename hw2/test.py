# ex1
print(1 + 2*3)
print((3*3 + 4*4) // 5)
print(10-3-4)

# ex2
print(not True and 1//0==0)
print(1<2)
if False or True:
    print("ok")
else:
    print("oups")

# ex3
x = 41
x = x+1
print(x)
b = True and False
print(b)
s = "hello" + " world!"
print(s)

#ex4
def fact(n):
    if n <= 1: return 1
    return n * fact(n-1)
print(fact(10))