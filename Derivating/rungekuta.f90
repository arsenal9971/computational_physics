!Resolver con Runge-Kuta Q'=(V(0)-Q/C)/R la de un un capacitor
real k1,k2,l1,l2,x(0:10000000),t(0:10000000)
x(0)=0.
t(0)=0.
v0=10.
c=0.000001
R=1000.
h=0.0001
do i=1, 100000
k1=h*(v0-x(i-1)/c)/R
k2=h*(v0-(x(i-1)+k1)/c)/R
t(i)=i*h
x(i)=x(i-1)+0.5*(k1+k2)
end do

do i=0, 100000
   print *, t(i),x(i)
end do

end
