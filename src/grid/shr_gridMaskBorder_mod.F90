!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskBorder_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridMaskBorder defines which gridcells are border
!> A border gridcell won't be used as it is considered outside of bounds
!>
!------------------------------------------------------------------------------
module shr_gridMaskBorder_mod
  use SHR_error_mod, only: raiseError
  !use SHR_precision_mod, only: sp

  use shr_strings_mod, only: string
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridMaskEnabled_mod, only: shr_gridMaskEnabled

  implicit none

  public :: shr_gridMaskBorder

  logical, parameter :: ISDEBUG = .false.

  type, extends(shr_gridMask) :: shr_gridMaskBorder
  contains
    procedure :: isValid => isValid_gridMask
  end type shr_gridMaskBorder

contains


  logical function isValid_gridMask(self, enabledMask)
    !< true if 'enabledMask' enabled gridcell fit into self valud gridcells
    !< true if 'enabledMask' gridMask true gridcells also match self mask array
    !<
    !< self (TT,FF) % isMaskIncluded(FF,TF) -> true
    !< self (TT,FF) % isMaskIncluded(TF,FT) -> false
    !<
    !< 'self' matches with 'enabledMask' gridcells?
    !<
    class(shr_gridMaskBorder), intent(in) :: self
    class(shr_gridMask), intent(in) :: enabledMask !< other
    type(shr_gridMask) :: disabledBorderMask, overlappingMask

    disabledBorderMask = self
    overlappingMask = (disabledBorderMask .and. enabledMask)
    !< no true value?
    isValid_gridMask = .not. (overlappingMask % any())
  end function isValid_gridMask

end module shr_gridMaskBorder_mod

