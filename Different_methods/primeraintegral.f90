program primeraintegral
!Este programa aproxima una integral doble con la regla del punto medio
dimension x(50000),y(50000),xmedio(50000),ymedio(50000)
N=2000
M=2000
hx=0.001!Definimos el salto en x y en y
hy=0.001
suma=0. !Inicializamos la integral en x
!Generamos los puntos del intervalo en x y en y, y sus puntos medios
do i=0, N
   x(i)=i*hx
end do  
do i=1,N
   xmedio(i)=(x(i-1)+x(i))/2.
end do
do j=0, M
   y(j)=j*hx
end do
do j=1,M
  ymedio(j)=(y(j-1)+y(j))/2
end do
do i=1, N !sumamos sobre x de 0 a 2
   do j=1001,M !sumamos sobre y de 0 a 1
      suma=suma+(xmedio(i)-3*ymedio(j)**2)*(x(i)-x(i-1))*(y(j)-y(j-1))!Hacemos la suma de Riemann
end do
end do
print*,"La integral de x^2-3y^2 en [0,2]x[1,2] es=", suma 
end program primeraintegral
           

