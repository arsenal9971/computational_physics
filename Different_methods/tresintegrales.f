                dimension x(0:2000),fx(0:2000)
c                common h
c                a=0.
c                b=3.
                n=100
                open(1,file='seno.dat',status='old')
                xinte=0. !Riemann
                xinte1=0. !Paralelogramo
                xinte2=0.  !Simpson
                do i=0,n
c                write(6,*)i
                read(1,*)x(i),fx(i)
                write(6,*) i,x(i),fx(i)
                end do
                
                a=x(0)
                b=x(n)
                h=(b-a)/n
                
                do i=0,n-1
                xinte=fx(i)*h+xinte
                xinte1=(h*(fx(i+1)+fx(i)))/2. +xinte1
                end do
                ndiv=n/2
                xdiv=n/2.
                dif=xdiv-ndiv
               if(dif.eq.0) goto 333
    		goto 222
333           nsimpson=n/2
               do j=1,nsimpson
                 ni=2*(j-1)
               xx0=x(ni)
               xx1=x(ni+1)
               xx2=x(ni+2)
               fxx0=fx(ni)
               fxx1=fx(ni+1)
               fxx2=fx(ni+2)
               call cuadratica(xx0,xx1,xx2,fxx0,fxx1,fxx2,a,b,c)
               print *,a,b,c
               xinte2=a*(xx2-xx0)+b*(xx2**2.-xx0**2.)/2.+c*(xx2**3.-xx0*
     /*3.)/3.+xinte2
               end do          
c                dif=(9.-xinte)/9.0
c                dif1=(9.-xinte1)/9.0
c                dif2=
               print *,xinte,xinte1,xinte2
               stop
222           n1=n-1
               nsimpson=n1/2
               do j=1,nsimpson
                 ni=2*(j-1)
               xx0=x(ni)
               xx1=x(ni+1)
               xx2=x(ni+2)
               fxx0=fx(ni)
               fxx1=fx(ni+1)
               fxx2=fx(ni+2)
               call cuadratica(xx0,xx1,xx2,fxx0,fxx1,fxx2,a,b,c)
               print *,a,b,c
               xinte2=a*(xx2-xx0)+b*(xx2**2.-xx0**2.)/2.+c*(xx2**3.-xx0*
     /*3.)/3.+xinte2
               end do
               
               
                
               xx0=x(n-2)
               xx1=x(n-1)
               xx2=x(n)
               fxx0=fx(n-2)
               fxx1=fx(n-1)
               fxx2=fx(n)
               call cuadratica(xx0,xx1,xx2,fxx0,fxx1,fxx2,a,b,c)
               print *,a,b,c
              xinte2a=a*(xx2-xx1)+b*(xx2**2.-xx1**2.)/2.+c*(xx2**3.-xx1*
     /*3.)/3.+xinte2
               print *,xinte,xinte1,xinte2a,xinte2a
               
                end
               
               
          subroutine cuadratica(x0,x1,x2,f0,f1,f2,xa,xb,xc)         
               
               xc=(f0*(x2-x1)+f1*(x0-x2)+f2*(x1-x0))/((x2-x1)*x0**2.+(x0
     /-x2)*x1**2.+(x1-x0)*x2**2.)
               xb=(f1-f0-xc*(x1**2. -x0**2.))/(X1-X0)
               xa=f0-xb*x0-xc*x0**2.
c               print *,xa,xb,xc               
               end
              
