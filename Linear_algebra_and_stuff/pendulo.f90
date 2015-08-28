program gas
!Hector Andrade Loarca
!en este programa se calcula el volúmen específico de un cierto gas encontrando las raices de la ecuación de Van der Waals por el metodo de biseccion
print*, 'Primer punto' !Leemos el intervalo donde quiero que encuentre las raices
read*,x0
print*, 'Punto final'
read*, xn
print*,'Número de divisiones'
read*,N

h=(xn-x0)/N

do i=0,n-1 !empezamos buscando las raices por medio del metodo de biseccion
x=i*h
x1=(i+1)*h

          fx=fun(x)
           fx1=fun(x1)
        prod=fx*fx1
         if(prod.lt. 0.) go to 666 !vemos cuando hay un cambio de signo
       go to 7
666 print*, 'hay una raiz entre ',x,'y',x1 !imprimimos las raices
7 continue
end do
end program gas

function fun(z) !Definimos la funcion con la ecuacion de van der waals
fun=0.0052-0.2041*z+z**2.
end
