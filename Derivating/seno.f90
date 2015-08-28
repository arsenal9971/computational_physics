program seno
pi=4.*atan(1.)
print *, pi
h=pi/100.
open(11, file="seno.dat", status="new")
do i=0, 200
   x=i*h
   sen=sin(x)
   print * , x, sen
   write(11,*)x, sen
end do
end program seno
