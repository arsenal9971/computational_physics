	dimension  x(0:10000000), y(0:10000000)
	
	x01=1.
	y01=5.
	m1=5.
	m2=1.
	l=10.
	
	v01=7.
	v02=-1.
	deltat=0.01
	
	x(0)=x01
	y(0)=y01
	v0x=v01
	v0y=v02
	print*, '-----------i--------ti-----------x(i)--------------y(i)'	
	print*,             0,        0,          x(0),             y(0)
	do i =1,100
	
	ti=i*deltat
	x(i)=x(i-1)+v0x*deltat
	y(i)=y(i-1)+v0y*deltat
	
	if(y(i)-x(i)<0)then
	v0x=(2*m2*v02+(m1-m2)*v01)/(m1+m2)
	v0y=(2*m1*v01+(m2-m1)*v02)/(m1+m2)
	end if
	
	if(x(i)<0) then
	v0x=-v0x
	end if
	
	if(y(i)>l) then
	v0y=-v0y
	end if
	
	print*, ti,x(i),y(i)
	
	end do

	end
	
