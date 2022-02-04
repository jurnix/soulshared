!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_logical_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!
!> logical type extension
!>
!------------------------------------------------------------------------------
!
module shr_logical_mod

  use shr_error_mod, only: raiseError

  implicit none 


  logical, parameter :: IS_DEBUG = .false.

contains

  function shr_logical_cast(obj) result (l)
    !< cast unlimited polymorphic 'obj' into logical type
    !< move to logical_mod.F90
    !< type extensions
    class(*), intent(in) :: obj
    logical :: l !< output

    select type(o => obj)
    type is (logical)
      l = o
    class default
      call raiseError(__FILE__, "shr_logical_cast", &
          "Unexpected type found instead of 'logical'")
    end select
  end function shr_logical_cast

end module shr_logical_mod
