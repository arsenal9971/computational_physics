program integraldoble

dimension x(0:50000),fc(0:50000),fx1(0:50000),fx2(0:50000)

hx=0.01
do i=1, 101 !definamos el dominio de integración
   j=i-1
   x(i)=j*hx
   fx(i)=x(i)
   fx2(i)=0
   !print *, fx(i),fx2(i)fx(i)-fx2(i)
end do

!Calculemos el area con una integal doble 
area=0.
ny=1000
do=1,10000
   hy=(fx(i)-0)/ny
   do j=1, ny
      fx1(j)=(j*hy)
      y=j*hy
      area=area+hy*hx
   end do
end do

123 format(E12.5)
write(6,123) area

end program integraldoble
