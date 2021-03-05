subroutine handleErr(status,errcode)
!----------------------------------------------------------------
! This routine reports the netCDF error and stops the program
!----------------------------------------------------------------

use netcdf

implicit none

integer            :: status
character(len=*)   :: errcode
character(len=600) :: msg

msg = nf90_strerror(status) 

write(0,*) 'Program stopping because of netCDF reported error =',trim(errcode),&
           ':',trim(msg)

stop

end subroutine handleErr
