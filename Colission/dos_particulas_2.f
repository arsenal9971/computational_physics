!Hector Andrade Loarca, Juan Carlos Del Valle, Rosalio Alejandro Reyes
	dimension  x(0:10000000), y(0:10000000)
	dimension  v0x(0:1000000), v0y(0:100000000)
	!Definimos las constantes
	x01=1.
	y01=5.
	m1=100.
	m2=1.
	l=10.
	
	v01=30.
	v02=-100.
	deltat=0.0001
	
	x(0)=x01
	y(0)=y01
	v0x(0)=v01
	v0y(0)=v02
!Calculamos las posiciones de las particulas
	do i =1,1000
	
	ti=i*deltat
	x(i)=x(i-1)+v0x(i-1)*deltat
	y(i)=y(i-1)+v0y(i-1)*deltat
	
	if(y(i)-x(i)<0)then
	
	v0x(i)=(2*m2*v0y(i-1)+(m1-m2)*v0x(i-1))/(m1+m2)
	v0y(i)=(2*m1*v0x(i-1)+(m2-m1)*v0y(i-1))/(m1+m2)
	
	else
	
	
	if(x(i)<0)then
	v0x(i)=-v0x(i-1)
	else
	v0x(i)=v0x(i-1)
	end if
	

	if(y(i)>l) then
	v0y(i)=-v0y(i-1)
	else
	v0y(i)=v0y(i-1)
	end if

	
	end if
	
	print*, ti,x(i),y(i)

	end do

	end
	
