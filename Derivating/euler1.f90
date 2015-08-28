     !resolver y'+xy-sen(x)=0
     dimension x(0:100000), y(0:100000)
     
     y(0)=1.
     x(0)=0.
     h=0.0001
     do i=1,100000
        x(i)=h*i
        y(i)=y(i-1)+h*(sin(x(i-1))-y(i-1)*x(i-1))
     end do

     do i=0,100000
        print *, x(i),y(i)
     end do
     end 
