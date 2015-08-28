program sustitucion
print *, "Dame el punto de inicio"
read *,x0
delta=0.0001

10 x1=f(x0)
print *,x1
dif=abs(x1-x0)
x0=x1
if (dif.gt.delta) go to 10

print *, "la raiz es", x1
end program sustitucion

function f(x)
f=(2.-exp(x)-x**2.)/3.
return
end function

