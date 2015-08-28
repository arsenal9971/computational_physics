!donde tiene raices la siguiente funcion?
print*, 'Primer punto'
read*,x0
print*, 'Punto final'
read*, xn
print*,'Número de divisiones'
read*,N

h=(xn-x0)/N

do i=0,n-1
x=i*h
x1=(i+1)*h

          fx=fun(x)
           fx1=fun(x1)
        prod=fx*fx1
         if(prod.lt. 0.) go to 666
       go to 7
666 print*, 'hay una raiz entre ',x,'y',x1
7 continue
end do
end

function fun(z)
fun=z**4. -10.*z**3. +15.*z**2. -7.*z +1.
end
