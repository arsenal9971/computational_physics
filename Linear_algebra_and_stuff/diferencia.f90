program Diferencia
do j=2,100
n=j
do i=2, n-1
   ndiv=n/i
   div=(n*1.)/i
   dif=div-ndiv
   if(dif.eq.0.) goto 113
end  do
print *, n, ' es primo'
stop

end do
113 print *, n, ' no es primo'

end program Diferencia
