program Archivos

  implicit none
  integer :: k
  integer, parameter :: N=100 ! Esta variable ya no se puede cambiar
  real :: vector(N) 

  open(unit=11, file="datos.txt", status= "replace")

  do k=1, N
     write(11,*), real(k), exp(real(k))
  end do

  close(unit=11)

  open(unit=12, file="datos.txt", status="old") !pongo old para que sea solo lectura
  do k=1, N
     read(12,*) vector(k) !Le meto los nùmeros a cada endtrada del vector

  end do

close(unit=12)
print *, vector
end program Archivos
