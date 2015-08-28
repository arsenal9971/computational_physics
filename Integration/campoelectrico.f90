program campoelectrico !campoelectrico de una circunferencia de radio 1 densidad de carga constane

dimension x(0:50000), fx(0:50000), fx1(0:50000), fx2(0:50000)
dimension y(0:50000)

hx=0.0001 !se define el salto
do i=0, 20000 !se define nuestra region
   x(i)=-1.+i*hx !region en que varia la x de (-1 a 1)
   fx(i)=sqrt(1.-x(i)*x(i))!para un circulo de radio 1
   fx2(i)=-fx(i)
! print *, fx(i), fx2(i), fx(i)-fx2(i), imprimir la regiòn de integraciòn

end do

!inicializamos las variables de integracion
campox=0.
campoy=0.
ny=100 !partimos y en 100 intervalos 
densi=1.
x0=2.!posicion del punto de observacion
y0=2.
do i=1,20000
   hy=(fx(i)-fx2(i))/ny !partimos verticalmente
   do j=1, ny
     ! fx1(j)=(j*hy)
      y(j)=fx2(i)+j*hy !sumamos los intervalitos
      tam=sqrt((x0-x(i))**2.+(y0-y(j))**2.)!tamaño del vector
      tam3=tam**3
      campox=campox+(densi*hx*hy*(x0-x(i)))/tam3 !coordenadas del campo
      campoy=campoy+(densi*hx*hy*(y0-y(j)))/tam3
      end do
end do
 
124 format('La coordenada x del campo es  ',F12.5)
125 format('La coordenada y del campo  ', F12.5)
write(6,124) campox
write(6,125) campoy
end program campoelectrico
