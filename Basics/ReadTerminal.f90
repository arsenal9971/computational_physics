program ReadTerminal

  implicit none
  real :: x, y
  integer :: k

  print *,
  print *, "************************"
  print *, "Conversión a Kelvin, dame una temperatura en Celsius"

  read(*,*) x  !para que lo lea en la terminal y lo guarde en x
  print *, x+273.15 !imprimer lo que se escribió desde la terminal

  print *, "Dame tres números"
  do k=1,3
     read(*,*) y
     print *, "Este es el numero:", y
  end do
     
end program ReadTerminal
