!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridAxesCell_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridAxesCell describes a 1 dimension gridcell 
!> 
!------------------------------------------------------------------------------
module shr_gGridAxesCell_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  public :: shr_gGridAxesCell

  logical, parameter :: ISDEBUG = .false.


  type shr_gGridAxesCell
    type(shr_gGridAxesBounds), allocatable :: bounds
    real(kind=sp) :: center
  contains
    procedure :: init => gGridAxesCell_initialize 
  end type shr_gGridAxesCell

contains

  subroutine gGridAxesCell_initialize(self, center, bounds)
    !< gGridAxesCell initialization
    !< startCoord > endCoord
    class(shr_gGridAxesCell), intent(inout) :: self
    real(kind=sp), intent(in) :: center
    type(shr_gGridAxesBounds), intent(in) :: bounds
    self % center = center
    allocate(self % bounds, source = bounds)
  end subroutine gGridAxesCell_initialize


end module shr_gGridAxesCell_mod 

