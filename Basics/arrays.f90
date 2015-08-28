program Arrays

  implicit none
  real :: vector(10)  ! Array de 10 elementos
  integer :: vector2(10)
  real :: matrix(3,3) !Matriz de 3x3
  integer :: j,k

  !Inicializacion del array 
  do k=1, 10
     vector(k) = k
  end do

  ! Impresión del Array
  do k=1, 10
     print *, vector(k)
  end do

  !Asignación de entradas a la Matriz 
  do j=1, 3
     do k=1, 3
        matrix(j,k) = (-1)**(j+k)
        end do
     end do
     
     print *, matrix

end program Arrays

       

