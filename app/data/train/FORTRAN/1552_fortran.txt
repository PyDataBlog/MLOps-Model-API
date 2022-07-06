subroutine checa_parametros()

use numeros_globales
implicit none

if (xmax.le.xmin) then
    print *,'Error xmax<=xmin'
    stop
end if

end subroutine checa_parametros