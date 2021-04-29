!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : error_mod 
!
!> @author
!> 
!
! DESCRIPTION:
!> Provide a standarized subroutine call 
!------------------------------------------------------------------------------
!
module error_mod

  use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
  use system_mod, only: abort

  implicit none 

  public :: raiseError, raiseUnexpectedError, raiseNotImplementedError

contains

  subroutine raiseNotImplementedError()
    !< customized error to raise when implementation is not yet done
    call raiseError(__FILE__, "raiseUnexpectedError", "Feature not yet implemented")
  end subroutine raiseNotImplementedError


  subroutine raiseUnexpectedError()
    !< customized error to raise when it should never take place
    call raiseError(__FILE__, "raiseUnexpectedError", "Unexpected condition reached")
  end subroutine raiseUnexpectedError


  subroutine raiseError(filename, subrname, message, mesgmed, mesglong, anmesg, withTraceback)
    !< stop the execution with an error
    character(len=*), intent(in) :: filename !< filename's calleer name
    character(len=*), intent(in) :: subrname !< subroutine's calleer name
    character(len=*), intent(in) :: message !< main error message
    character(len=*), optional, intent(in) :: mesgmed !< message extension 1
    character(len=*), optional, intent(in) :: mesglong !< message extension 2
    character(len=*), optional, intent(in) :: anmesg !< message extension 3
    logical, optional, intent(in) :: withTraceback !< stop the simulation by raising a traceback

    write(stderr, *) "Error at file: ", filename
    write(stderr, *) "      at subr: ", subrname
    write(stderr, *) "      message: ", message
    if (present(mesgmed)) then
      write(stderr, *) "             : ", mesgmed
    endif
    if (present(mesglong)) then
      write(stderr, *) "             : ", mesglong
    endif
    if (present(anmesg)) then
      write(stderr, *) "             : ", anmesg
    endif

    if (present(withTraceback)) then
      if (withTraceback)  call abort()
    endif

    error stop 1
  end subroutine raiseError


end module error_mod
