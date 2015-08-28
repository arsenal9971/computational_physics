program Programa1f90

  

  integer :: name
  real :: x
  complex :: z
  character(len=100) :: str
  logical :: value
  
  integer :: k

  name = 345
  x = 1.37e9
  z = cmplx(x)
  str = "1e2"
  value = .true. ! o .false. entre puntos variables lógicas
  
  print *, "Este es mi primer programa serio de Fortran"
  
  k=100

  if(k > 100) then 
     print *, "k es mayor que 100"

  else
     print *, "k es menor o igual que 100"
   end if

end program Programa1f90
