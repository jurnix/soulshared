!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDescriptor_stub 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gGridDescriptor stub
!------------------------------------------------------------------------------
module shr_gGridDescriptor_stub

  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  !< dependencies
  use shr_precision_mod, only: sp
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_strings_mod, only: string

  implicit none

  private
  public :: shr_gGridDescriptorEmptyStub

  !< stub
  type, extends(shr_igGridDescriptor) :: shr_gGridDescriptorEmptyStub
  contains
    procedure :: initialize
    procedure :: initialize_simple

    procedure :: getResolution
    procedure :: getBounds
    procedure :: getLatAxis
    procedure :: getLonAxis
    procedure :: toString

    procedure :: equal
    !generic :: operator(==) => eq_gridDescriptor

    procedure :: combine
    !generic :: operator(+) => gridDescriptor_combine

    procedure :: fitsIn
  end type shr_gGridDescriptorEmptyStub

contains

  elemental subroutine initialize(self, resolution, bounds, latAxis, lonAxis)
    !< grid descriptor initialization
    class(shr_gGridDescriptorEmptyStub), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gridBounds), intent(in) :: bounds
    type(shr_gGridAxes), intent(in) :: latAxis
    type(shr_gGridAxes), intent(in) :: lonAxis
  end subroutine initialize


  subroutine initialize_simple(self, resolution, bounds)
    !< initialize with fundamental arguments
    class(shr_gGridDescriptorEmptyStub), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gridBounds), intent(in) :: bounds
  end subroutine initialize_simple


  elemental real(kind=sp) function getResolution(self)
    !< it returns class resolution
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
  end function getResolution


  elemental impure type(shr_gridBounds) function getBounds(self)
    !< return class bounds
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
  end function getBounds


  elemental type(shr_gGridAxes) function getLatAxis(self)
    !< return class bounds
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
  end function getLatAxis


  elemental type(shr_gGridAxes) function getLonAxis(self)
    !< return class bounds
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
  end function getLonAxis


  elemental logical function equal(self, other)
    !<
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
  end function equal


  function combine(self, other) result (newGDescriptor)
    !< combine self and other into a new shr_gGridDescriptorEmptyStub
    !< - resolution must be the same
    !< - extends bounds to largest axis
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
    class(shr_iGGridDescriptor), allocatable :: newGDescriptor !< output
  end function combine


  logical function fitsIn(self, other)
    !< true if 'other' grid bounds fits in 'self'
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
  end function fitsIn


  type(string) function toString(self)
    !<
    class(shr_gGridDescriptorEmptyStub), intent(in) :: self
  end function toString

end module shr_gGridDescriptor_stub

