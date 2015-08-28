program integral

  dimension x(0:1000000), fx(0:1000000)
  n=1000
  h=0.01
  do i=0, n
     x(i)=i*h
     fx(i)=x(i)*x(i)
  end do
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

  
