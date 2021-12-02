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

    procedure :: getResolution
    procedure :: getBounds
    procedure :: getLatAxis
    procedure :: getLonAxis
  end type shr_gGridDescriptor

contains


  elemental subroutine gGridDescriptor_initialize(self, resolution, bounds, latAxis, lonAxis)
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


  elemental real(kind=sp) function getResolution(self)
    !< it returns class resolution
    class(shr_gGridDescriptor), intent(in) :: self
    getResolution = self % resolution
  end function getResolution


  elemental type(shr_gridBounds) function getBounds(self)
    !< return class bounds
    class(shr_gGridDescriptor), intent(in) :: self
    getBounds = self % bounds
  end function getBounds


  elemental type(shr_gGridAxes) function getLatAxis(self)
    !< return class bounds
    class(shr_gGridDescriptor), intent(in) :: self
    getLatAxis = self % latAxis
  end function getLatAxis


  elemental type(shr_gGridAxes) function getLonAxis(self)
    !< return class bounds
    class(shr_gGridDescriptor), intent(in) :: self
    getLonAxis = self % lonAxis
  end function getLonAxis


end module shr_gGridDescriptor_mod 

