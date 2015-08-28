program newton

print *, "Inicio"
read *,x

5 fx=F(x)
 if(abs(fx).lt.0.0001) goto 10
delta=-fx/Fd(x)
x=x+delta
print *,x,fx
goto 5
10 end program newton
function F(x)
F=sin(x)-x/2
return
end 

function Fd(x)
Fd=cos(x)-1./2
return
end
