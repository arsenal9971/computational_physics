program centromasa2 !centro de masa de un circulo (cuadrado)

dimension x(0:50000), fx(0:50000), fx1(0:50000), fx2(0:50000)
dimension y(0:50000)

hx=0.0001 !se define el salto
do i=0, 20000 !se define nuestra region
   x(i)=-1.+i*hx !region en que varia la x de (-1 a 1)
 !  fx(i)=sqrt(1.-x(i)*x(i)) ,para un circulo de radio 1
   fx(i)=2. !cuadrado de 2x2
   fx2(i)=0.
! print *, fx(i), fx2(i), fx(i)-fx2(i), imprimir la regiòn de integraciòn

end do

area=0.!inicializamos las variables de integracion
areax=0.
areay=0.
ny=100 !partimos y en 100 intervalos 
!densi=1.

do i=1,20000
   hy=(fx(i)-fx2(i))/ny !partimos verticalmente
   do j=1, ny
     ! fx1(j)=(j*hy)
      tam=sqrt(x(i)**2.+y(j)**2.)
      densi=tam
      y(j)=fx2(i)+j*hy !sumamos los intervalitos
      area=area+densi*hy*hx !area de los rectangulos
      areax=areax+densi*hx*hy*x(i)
      areay=areay+densi*hx*hy*y(j)
      end do
end do
123 format('La masa total es  ',F12.5) !le damos formato a cada dato 
124 format('La coordenada x del centro de masa es  ',F12.5)
125 format('La coordenada y del centro de masa es  ', F12.5)
write(6,123) area
write(6,124) areax/area
write(6,125) areay/area
end program centromasa2
