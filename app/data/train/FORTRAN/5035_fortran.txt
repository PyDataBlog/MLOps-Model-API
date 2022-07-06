!Fortran permite la conversión entre tipos. Cuando se operan reales con enteros o complejos, Fortran los convierte de manera automática:
!	*Los enteros son convertidos en reales o complejos
!	*Los reales son convertidos en complejos
!	*Los reales o complejos son convertidos en la clase (kind) más alto.
!	*En la asignación de valores (=), la parte derecha es evaluada en el tipo y clase que corresponde, luego es convertida al tipo y
!	 clase de la variable del lado izquierdo

program conversion_tipos
integer :: int = 6
real :: r_var = 3.5
complex :: z = (2, 5.9)

print *, "Complex + integer: ", z + int
print *, "Complex + real: ", r_var + z
print *, "Real + integer: ", r_var + int


end program conversion_tipos