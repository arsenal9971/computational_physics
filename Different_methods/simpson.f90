program simpson
dimension x(0:50000),fx(0:50000)
open(1,file='seno.dat', status='old')
n=100
nsimpson=n/2
!se realiza la lectura de archivo
do k=0, n
   read(1,*) x(k), fx(k)
end do
!Ajuste de tres puntos
simp=0.
do i=1,nsimpson
   j=2*(i-1)
   xx0=x(j)
   ff0=fx(j)
   xx1=x(j+1)
   ff1=fx(j+1)
   xx2=x(j+1)
   ff2=fx(j+2)
   call cuadratica(xx0,xx1,xx2,ff0,ff1,ff2,aa,bb,cc)
!   print *, i, aa,bb,cc
   spara=aa*(xx2-xx0)
   sparb=(bb/2.)*(xx2**2.-xx0**2.)
   sparc=(cc/3.)*(xx2**3.-xx0**3.)
   simp=simp+spara+sparb+sparc
end do
print *,simp
end program simpson

subroutine cuadratica(x0,x1,x2,f0,f1,f2,a,b,c)
c=(f0-2.*f1+f2)/(x0**2.-2.*x1**2.+x2**2.)
b=(f1-f0+c*(x0**2.-x1**2.))/(x1-x0)
a=f0-b*x0-c*x0**2
end subroutine 
