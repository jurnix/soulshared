!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskEnabled_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridMaskEnabled defines which gridcells are enabled
!>
!------------------------------------------------------------------------------
module shr_gridMaskEnabled_mod
  !use SHR_error_mod, only: raiseError
  !use SHR_precision_mod, only: sp

  use shr_gridMask_mod, only: shr_gridMask, shr_IgridMask

  implicit none

  public :: shr_gridMaskEnabled

  logical, parameter :: ISDEBUG = .false.

  type, extends(shr_gridMask) :: shr_gridMaskEnabled
  contains
    procedure :: initialize_from_gridMask
    generic :: init => initialize_from_gridMask
  end type shr_gridMaskEnabled

contains

  subroutine initialize_from_GridMask(self, gmask)
    !< convert gmask into gridMaskEnabled
    class(shr_gridMaskEnabled), intent(inout) :: self
    class(shr_igridMask), intent(in) :: gmask
    self % gridDescriptor = gmask % getGridDescriptor()
    self % mask = gmask % get()
  end subroutine initialize_from_GridMask

end module shr_gridMaskEnabled_mod

