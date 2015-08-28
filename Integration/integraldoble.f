         dimension x(50000),fx(50000),fx1(50000),fx2(50000), y(50000)
         hx=0.0001           
         do i=1,10001
           j=i-1
           x(i)=j*hx
           fx(i)=x(i)
           fx2(i)=0
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
               area=area+hy*hx*x(i)*x(i)*y(j)  
             end do
         end do
123      format(E12.5)         
         write(6,123) area
         end        
           
