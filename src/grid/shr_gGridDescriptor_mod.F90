!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridDescriptor_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridDescriptor describes main features of a grid 
!> 
!------------------------------------------------------------------------------
module shr_gGridDescriptor_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridAxes_mod, only: shr_gGridAxes

  implicit none

  public :: shr_gGridDescriptor

  logical, parameter :: ISDEBUG = .false.


  type shr_gGridDescriptor
    real(kind=sp) :: resolution = -1
    type(shr_gridBounds), allocatable :: bounds
    type(shr_gGridAxes), allocatable :: latAxis
    type(shr_gGridAxes), allocatable :: lonAxis
  contains
    procedure :: init => gGridDescriptor_initialize 
  end type shr_gGridDescriptor

contains


  subroutine gGridDescriptor_initialize(self, resolution, bounds, latAxis, lonAxis)
    !< grid descriptor initialization
    class(shr_gGridDescriptor), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gridBounds), intent(in) :: bounds
    type(shr_gGridAxes), intent(in) :: latAxis
    type(shr_gGridAxes), intent(in) :: lonAxis
    self % resolution = resolution
    allocate(self % bounds, source = bounds)
    allocate(self % latAxis, source = latAxis)
    allocate(self % lonAxis, source = lonAxis)
  end subroutine gGridDescriptor_initialize


end module shr_gGridDescriptor_mod 

