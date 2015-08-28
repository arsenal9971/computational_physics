     !resolver y''-2y'=3cos(xy)
     dimension x(0:100000), y(0:100000), z(0:1000000)
     
     y(0)=1.
     z(0)=0.
     x(0)=0.
     h=0.001
     do i=1,10000
        x(i)=h*i
        y(i)=y(i-1)+h*z(i-1)
        z(i)=z(i-1)+h*(2.*z(i-1)+3.*cos(x(i-1)*y(i-1)))
        print *, x(i),y(i),z(i)
     end do
     end 
