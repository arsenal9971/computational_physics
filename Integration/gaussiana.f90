program integral

  dimension x(-1000000:1000000), fx(-1000000:1000000)
  n=200
  h=0.2

  do i=-100,100
     x(i)= i*h
     fx(i)=exp(-x(i)*x(i))
     end do
!Empecemos la integral con sumas de Riemann
  xinte=0
  do i=-100,100
     xinte=xinte+fx(i)*h
     
  end do

!Empecemos la integral con regla de trapecio
xinte1=0
do i=-100,100
   xinte1=xinte1+h*0.5*(fx(i)+fx(i+1))
end do

print*, xinte, xinte1

end program integral

  
