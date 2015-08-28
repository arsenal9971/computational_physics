program centromasa

  dimension x(0:10000000),fx(0:10000000)
  real l0
  xlambda=1 !densidad lineal
  l0=6
  n=100
  h=l0/100
  do i=0,n
     x(i)=-3.+i*h
     fx(i)=xlambda*x(i)
     end do
     xinte=0.
     xintem=0
  do i=1,n
     xinte=xinte+fx(i)*h
     xintem=xintem+xlambda*h 
 end do

  xinte1=0.
  xintem1=0
  do i=0, n-1
     xinte1=xinte1+h*0.5*(fx(i)+fx(i+1))
     xintem1= xintem1+h*0.5*(xlambda+xlambda)
  end do
 xcm=xinte/xintem
 xcm1=xinte1/xintem1
print *, xcm,xcm1,xintem,xintem1

end program centromasa
