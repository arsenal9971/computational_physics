program coseno
pi=4*atan(1.)
h=pi/100

open(11,file='coseno.dat',status='replace')

do i=0, 200

   x=i*h
   cose=cos(x)
   write(11,*) x, cose
end do

end program coseno
