!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : system_mod 
!
!> @author
!> 
!
! DESCRIPTION:
!>
!>
!------------------------------------------------------------------------------
!
module system_mod

  use, intrinsic :: iso_c_binding, only: c_int

  implicit none 

  public :: abort, fortSleep

  interface
    subroutine abort() bind(C, name="abort")
    end subroutine
  end interface

  interface
    !  should be unsigned int ... not available in Fortran
    !  OK until highest bit gets set.
    function FortSleep (seconds)  bind ( C, name="sleep" )
          import
          integer (c_int) :: FortSleep
          integer (c_int), intent (in), VALUE :: seconds
    end function FortSleep
  end interface

contains

end module system_mod
