program Derivada 
dimension x(0:1000),fx(0:1000) !Definimos x(i) y fx(i)
!xi=8. ! Defino los valores de cada punto y su imagen f(x)=x**2, 
!fxi=64.
!xi1=9.
!fxi1=81.
!xi2=10.
!fxi2=100.
  open(11,file='seno.dat', status='old')
  open(12,file='senoresult.dat', status='new')
  do i=0, 200
     read(11,*) x(i),fx(i)
   !  print *, i,x(i),fx(i)
  end do    
   
h=x(10)-x(9)
do i=0, 198
dfi=(-3.*fx(i)+4.*fx(i+1)-fx(i+2))/(2.*h)
d2fi=(fx(i)-2.*fx(i+1)+fx(i+2))/(h**2)
print *, x(i), fx(i), dfi, d2fi
!print *, "Segunda derivada", xi, fxi, d2fi
write(12,*) x(i),fx(i), dfi, d2fi
end do
end program Derivada
  
