         dimension x(0:50000),fx(0:50000),fx1(0:50000),fx2(0:50000)
         dimension y(0:50000)
         hx=0.0001           
         do i=1,1000
           
           x(i)=i*hx
           fx(i)=sqrt(1.-x(i)*x(i))
           fx2(i)=-fx(i)
c         print *,fx(i),fx2(i),fx(i)-fx2(i)
         end do
        
         area=0.
         ny=1000
         do i=1,10000
           hy=(fx(i)-fx2(i))/ny
             do j=1,ny
c                fx1(j)=(j*hy)
                y(j)=j*hy      
C         A continuacion definimos la funcion
               area=area+hy*hx  
             end do
         end do
 111     format(E12.5)         
         write(6,111) area
         end        
           
