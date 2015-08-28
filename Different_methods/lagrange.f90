program lagrange
dimension x(100), f(100)
open(1,file='seno11.dat', status='old')

do i=1,11
   read(1,*) x(i),f(i)
end do
pi=4.*atan(1.)
h=2.*pi/1000
do k=1, 1000
xa=k*h !hace la interpolacion en el punto 1.5
g=0.

do i=1, 11
   z=f(i) 
   do j=1, 11
      if(i.ne.j) z=z*(xa-x(j))/(x(i)-x(j))
      continue
   end do
   g=g+z
end do
write(6,*) xa,g
end do
end program lagrange
