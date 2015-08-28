program integral

  dimension x(0:1000000), fx(0:1000000)
  n=1000
  h=0.01
  do i=0, n
     x(i)=i*h
     fx(i)=x(i)*x(i)
  end do
!Empecemos la integral
  xinte=0
  do i=1,n
     xinte=xinte+fx(i)*h
     print*, xinte
  end do

end program integral

  
