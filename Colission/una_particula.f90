!Hector Andrade Loarca, Juan Carlos Del Valle, Rosalio Alejandro Reyes
program proyecto
!Este programa simula la dinámica de un gas unidimensional en una caja con una sola partícula
!Definimos los datos iniciales
x0=7.
l=10.
v=1.
h=0.01
tx=0.
tx1=0.
t=0.
!lanzamos la partícula hacia la derecha
do i=0, 1000
t=i*h
xi=x0+v*t
if (xi.gt.l) goto 1
print *,t, xi
end do
!Hacemos los rebotes con las paredes
1 do i=0, 1000
tx1=h*i
xi=l-v*tx1
if (xi.lt.0) goto 2
print *, tx1+t,xi
end do

2 do i=0,100
tx=i*h
xi=v*tx
if(xi.gt.l) goto 1
print *,tx1+tx+t, xi
end do

end program proyecto 
