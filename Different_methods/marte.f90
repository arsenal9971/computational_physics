program Marte
dimension t(0:1000),x(0:1000), y(0:1000), dx(0:1000), dy(0:1000), d2x(0:1000), d2y(0:1000) !Definimos t(i),x(i) y y(i)

  open(11,file='marte.dat', status='old')
  open(12,file='marteresultados.dat', status='replace')
  do i=0, 275
     read(11,*) t(i), x(i), y(i)
   end do    
   
h=t(2)-t(1) !defino el salto en el tiempo que es constente

!Calculo numéricamente las derivadas en x y y
do i=0, 273
dx(i)=(-3.*x(i)+4.*x(i+1)-x(i+2))/(2.*h)
d2x(i)=(x(i)-2.*x(i+1)+x(i+2))/(h**2)
dy(i)=(-3.*y(i)+4.*y(i+1)-y(i+2))/(2.*h)
d2y(i)=(y(i)-2.*y(i+1)+y(i+2))/(h**2)
write(12,13) t(i),x(i), y(i), dx(i), dy(i), d2x(i), d2y(i)
end do
13 Format(F8.3,1x,F8.3,1x,F8.3,1x,F8.3,1x,F8.3,1x,F8.3,1x,F8.3)
print*, "La aceleración en x es=", d2x(0)
print*, "La aceleración en y es=", d2y(0)
print*, "La velocidad inicial en x  es=", dx(0)
print*, "La velocidad inicial en y es=", dy(0)
end program Marte
  
