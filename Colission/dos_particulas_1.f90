!Simulacion numerica de la colision elastica de dos particulas en una caja unidimensional 

!Definicion de constantes 
real l, m1,m2


vi1=10. !Velocidad de la particula 1
vi2=9.  !Velocidad de la particula 2, sin incluir el sentido

!Inicializacion de tiempos en diferentes momentos del fenomeno
tk=0.
tp=0.
tl=0.
ts=0.
tr=0.
tx=0.
txx=0.

!Posiciones inciciales
xi1=1. !Particula 1
xi2=10.!Particula 2

!Longitud de la caja
l=10.

!Masas de cada particula.
m1=1. !Particula 1
m2=1. !Particula 2

!Incremento de tiempo
t=0.001

   do i=0, 1000
      tx=i*t
      x1=xi1+vi1*tx !Avance de la particula 1
      x2=xi2-1.*vi2*tx !Avance de la particula 2
      if((x2-x1).lt.0.) goto 5 !Condicion de choque
      print *, tx,x1,x2 !Impresion de posiciones en los respectivos tiepos
   end do


5      do i=0, 1000
          vf1=(((m1-m2)/(m1+m2))*vi1-2.*(m2/(m1+m2))*vi2) !Velocidad adquirida por la particula 1
          vf2=2.*(m1/(m1+m2))*vi1-((m2-m1)/(m1+m2))*vi2   !Velocidad adquirida por la particula 2
          tr=i*t
          x1r=x1+vf1*tr
          x2r=x2+vf2*tr
          if(((l-x1r).lt.0 .or. x1r.lt.0)) goto 6 !Posible caso: colision de la particula 1 con la pared derecha o colision de ambas particulas en la pared derecha, recordamos que son particulas puntuales
          if(((l-x2r).lt.0 .or. x2r.lt.0)) goto 7 !Posible caso: colision de la particula 2 con la pared derecha — colision de ambas particulas en la pared izquierda
          print*,ts+tk+tp+tl+tr+tx+txx,x1r,x2r !las sumas tienen el fin de hacer el tiempo continuo
       end do

6     do i=0, 1000        
         tk=i*t
         x1k=x1r-vf1*tk !colision de la particula 1, notese el cambio de direccion en la velocidad
         x2k=x2r+vf2*tk !la particula 2 se queda intacta y su avance es el mismo
         if((l-x2k).lt.0 .or. x2k.lt.0) goto 8 !Posible caso: colision de la particula 2 con alguna de las paredes 
         x1x=x1l !definicion de nuevas posiciones en una variable muda, la cual tiene el fin de poder ser reescrita
         x2x=x2l
         if((x2k-x1k).lt.0.) goto 10 !Posibilidad de una nueva colision entre las particulas
         print *,ts+tk+tp+tl+tr+tx+txx, x1k, x2k !se incluye el tiempo transcurrido
      end do

7     do i=0, 1000
         tl=i*t
         x1l=x1r+vf1*tl  !la particula 1 se queda intacta y su avance es el mismo
         x2l=x2r-vf2*tl  !colision de la particula 2, notese el cambio de direccion en la velocidad
         if(((l-x1l).lt.0 .or. x1l.lt.0)) goto 9 !Posible caso: colision de la particula 1, con alguna de las paredes
         x1x=x1l !definicion de nuevas posiciones en una variable muda, la cual tiene el fin de poder ser reescrita
         x2x=x2l
         if((x2l-x1l).lt.0.) goto 10 !Posibilidad de una nueva colision entre las particulas 
         print *,ts+tk+tp+tl+tr+tx+txx, x1l,x2l !se incluye el tiempo transurrido
      end do

8     do i=0, 1000
         ts=i*t
         x1s=x1k-vf1*ts !Mismo avance de la primera particula
         x2s=x2k-vf2*ts !Cambio en el avance de la segunda particula, dado el choque con la pared rebota
         x1x=x1s
         x2x=x2s
         if((x2s-x1s).lt.0.) goto 10 !Posibilidad de una nueva colision
         print *,ts+tk+tp+tl+tr+tx+txx, x1s,x2s !se incluye un nuevo tiempo trancurrido
      end do

9   do i=0, 1000
         tp=i*t
         x1p=x1l-vf1*tp !Colision de la particula 1, con la pared
         x2p=x2l-vf2*tp !mismo avance de la particula 2
         x1x=x1p
         x2x=x2p
         if((x2p-x1p).lt.0.) goto 10 !Posibilidad de una nueva colision
         print *,ts+tk+tp+tl+tr+tx+txx, x1p,x2p
    end do

10   do i=0, 100 !los casos conllevan a una nueva colision entre las particulas
        vff1=-(((m1-m2)/(m1+m2))*vf1+2.*(m2/(m1+m2))*vf2) !nuevas velocidades adquiridas utilizando las velocidades obtenidas anteriormente
        vff2=-2.*(m1/(m1+m2))*vf1+((m2-m1)/(m1+m2))*vf2
         txx=i*t
         x1r=x1x+vff1*txx !se utiliza el renombre de las variables mudas
         x2r=x2x+vff2*txx
         if(((l-x1r).lt.0 .or. x1r.lt.0)) goto 6 !reinicio de los ciclos: colision de la part“cula 1 con alguna de las paredes
         if(((l-x2r).lt.0 .or. x2r.lt.0)) goto 7 !reinicio de los ciclos  colision de la particula 2, con alguna de las paredes 
         print*,ts+tk+tp+tl+tr+tx+txx,x1r,x2r
    end do

end 
