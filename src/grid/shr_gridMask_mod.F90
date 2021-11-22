!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMask_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridMask is a part of a grid
!> 
!------------------------------------------------------------------------------
module shr_gridMask_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gridcell_mod, only: shr_gridcell

  implicit none

  public :: shr_gridMask

  logical, parameter :: ISDEBUG = .false.


  type shr_gridMask
    type(shr_gridBounds), allocatable :: bounds
    type(shr_gridcell), allocatable :: gridcells(:)
  contains
    procedure :: init => gridMask_initialize 
  end type shr_gridMask

contains

  subroutine gridMask_initialize(self, gridcells)
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    type(shr_gridcells), intent(in) :: gridcells(:)
    self % gridcells = gridcells
  end function gridMask_initialize


end module shr_gridMask_mod 

