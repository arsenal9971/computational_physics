program integral
  n=200
  dimension x(0:10000), fx(0:10000)
 ! n=1000
!  h=0.01
  open(1, file='seno.dat', status='old')

do i=0, n
    read(1,*) x(i), fx(i)
  end do
h=x(n)-x(n-1)
!Empecemos la integral con sumas de Riemann
  xinte=0
  do i=1,n
     xinte=xinte+fx(i)*h
     
  end do

!Empecemos la integral con regla de trapecio
xinte1=0
do i=0, n-1
   xinte1=xinte1+h*0.5*(fx(i)+fx(i+1))
end do

print*, xinte, xinte1

end program integral

  
