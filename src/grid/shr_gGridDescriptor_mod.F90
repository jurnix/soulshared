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
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_strings_mod, only: string

  implicit none

  public :: shr_gGridDescriptor, shr_iGGridDescriptor

  logical, parameter :: ISDEBUG = .false.


  !< interface
  type, abstract :: shr_iGGridDescriptor
  contains
    procedure(iface_initialize), deferred :: initialize
    procedure(iface_initialize_simple), deferred :: initialize_simple
    generic :: init => initialize, initialize_simple

    procedure(iface_getLatAxis), deferred :: getLatAxis
    procedure(iface_getLonAxis), deferred :: getLonAxis
    procedure(iface_fitsIn), deferred :: fitsIn
    procedure(iface_getResolution), deferred :: getResolution
    procedure(iface_getBounds), deferred :: getBounds

    procedure(iface_combine), deferred :: combine
    generic :: operator(+) => combine

    procedure(iface_equal), deferred :: equal
    generic :: operator(==) => equal
  end type shr_iGGridDescriptor


  abstract interface
    elemental subroutine iface_initialize(self, resolution, bounds, latAxis, lonAxis)
      import :: shr_iGGridDescriptor, sp, shr_gridBounds, shr_gGridAxes
      !< grid descriptor initialization
      class(shr_iGGridDescriptor), intent(inout) :: self
      real(kind=sp), intent(in) :: resolution
      type(shr_gridBounds), intent(in) :: bounds
      type(shr_gGridAxes), intent(in) :: latAxis
      type(shr_gGridAxes), intent(in) :: lonAxis
    end subroutine iface_initialize

    subroutine iface_initialize_simple(self, resolution, bounds)
      import :: shr_iGGridDescriptor, sp, shr_gridBounds
      !< initialize with fundamental arguments
      class(shr_iGGridDescriptor), intent(inout) :: self
      real(kind=sp), intent(in) :: resolution
      type(shr_gridBounds), intent(in) :: bounds
    end subroutine iface_initialize_simple

    function iface_combine(self, other) result (newGDescriptor)
      import :: shr_iGGridDescriptor
      !< combine self and other into a new shr_gGridDescriptor
      !< - resolution must be the same
      !< - extends bounds to largest axis
      class(shr_iGGridDescriptor), intent(in) :: self
      class(shr_iGGridDescriptor), intent(in) :: other
      class(shr_iGGridDescriptor), allocatable :: newGDescriptor !< output
    end function iface_combine

    elemental type(shr_gGridAxes) function iface_getLatAxis(self)
      import :: shr_iGGridDescriptor, shr_gGridAxes
      !< return class bounds
      class(shr_iGGridDescriptor), intent(in) :: self
    end function iface_getLatAxis

    elemental type(shr_gGridAxes) function iface_getLonAxis(self)
      import :: shr_iGGridDescriptor, shr_gGridAxes
      !< return class bounds
      class(shr_iGGridDescriptor), intent(in) :: self
    end function iface_getLonAxis

    logical function iface_fitsIn(self, other)
      import :: shr_iGGridDescriptor
      !< true if 'other' grid bounds fits in 'self'
      class(shr_iGGridDescriptor), intent(in) :: self
      class(shr_iGGridDescriptor), intent(in) :: other
    end function iface_fitsIn

    elemental real(kind=sp) function iface_getResolution(self)
      import :: shr_iGGridDescriptor, sp
      !< it returns class resolution
      class(shr_iGGridDescriptor), intent(in) :: self
    end function iface_getResolution

    elemental type(shr_gridBounds) function iface_getBounds(self)
      import :: shr_iGGridDescriptor, shr_gridBounds
      !< return class bounds
      class(shr_iGGridDescriptor), intent(in) :: self
    end function iface_getBounds

    elemental logical function iface_equal(self, other)
      import :: shr_iGGridDescriptor
      !< ==
      class(shr_iGGridDescriptor), intent(in) :: self
      class(shr_iGGridDescriptor), intent(in) :: other
    end function iface_equal
  end interface


  type, extends(shr_iGGridDescriptor) :: shr_gGridDescriptor
    real(kind=sp) :: resolution = -1
    type(shr_gridBounds), allocatable :: bounds
    type(shr_gGridAxes), allocatable :: latAxis
    type(shr_gGridAxes), allocatable :: lonAxis
  contains
    procedure :: initialize => gGridDescriptor_initialize
    procedure :: initialize_simple => gGridDescriptor_initialize_simple
    !generic :: init => gGridDescriptor_initialize, gGridDescriptor_initialize_simple

    procedure :: getResolution => gGridDescriptor_getResolution
    procedure :: getBounds => gGridDescriptor_getBounds
    procedure :: getLatAxis => gGridDescriptor_getLatAxis
    procedure :: getLonAxis => gGridDescriptor_getLonAxis

    procedure :: equal => gGridDescriptor_equal
    !generic :: operator(==) => eq_gridDescriptor

    procedure :: combine => gGridDescriptor_combine
    !generic :: operator(+) => gridDescriptor_combine

    procedure :: fitsIn => gGridDescriptor_fitsIn
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


  subroutine gGridDescriptor_initialize_simple(self, resolution, bounds)
    !< initialize with fundamental arguments
    class(shr_gGridDescriptor), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gridBounds), intent(in) :: bounds

    type(shr_gGridAxesBounds)  :: latAxisBounds, lonAxisBounds
    type(string) :: latName, lonName

    self % resolution = resolution
    allocate(self % bounds, source = bounds)

    call latAxisBounds % init(bounds % north, bounds % south)
    call lonAxisBounds % init(bounds % east, bounds % west)

    latName = string("latitude")
    allocate(self % latAxis)
    call self % latAxis % init(latName, resolution, latAxisBounds)

    lonName = string("longitude")
    allocate(self % lonAxis)
    call self % lonAxis % init(lonName, resolution, lonAxisBounds)
  end subroutine gGridDescriptor_initialize_simple


  elemental real(kind=sp) function gGridDescriptor_getResolution(self)
    !< it returns class resolution
    class(shr_gGridDescriptor), intent(in) :: self
    gGridDescriptor_getResolution = self % resolution
  end function gGridDescriptor_getResolution


  elemental type(shr_gridBounds) function gGridDescriptor_getBounds(self)
    !< return class bounds
    class(shr_gGridDescriptor), intent(in) :: self
    gGridDescriptor_getBounds = self % bounds
  end function gGridDescriptor_getBounds


  elemental type(shr_gGridAxes) function gGridDescriptor_getLatAxis(self)
    !< return class bounds
    class(shr_gGridDescriptor), intent(in) :: self
    gGridDescriptor_getLatAxis = self % latAxis
  end function gGridDescriptor_getLatAxis


  elemental type(shr_gGridAxes) function gGridDescriptor_getLonAxis(self)
    !< return class bounds
    class(shr_gGridDescriptor), intent(in) :: self
    gGridDescriptor_getLonAxis = self % lonAxis
  end function gGridDescriptor_getLonAxis


  elemental logical function gGridDescriptor_equal(self, other)
    !<
    class(shr_gGridDescriptor), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
    type(shr_gGridDescriptor) :: gDesc
    logical :: hasSameRes, hasSameLat, hasSameLon, hasSameBounds

    select type(o => other)
    type is (shr_gGridDescriptor)
      gDesc = o
    class default !< expected type not found
      gGridDescriptor_equal = .false.
      return
    end select

    hasSameRes = (self % resolution == gDesc % resolution)
    hasSameLat = (self % latAxis == gDesc % latAxis)
    hasSameLon = (self % lonAxis == gDesc % lonAxis)
    hasSameBounds = (self % bounds == gDesc % bounds)
    gGridDescriptor_equal = (hasSameRes .and. hasSameLat .and. &
                          hasSameLon .and. hasSameBounds)
  end function gGridDescriptor_equal


  function gGridDescriptor_combine(self, other) result (newGDescriptor)
    !< combine self and other into a new shr_gGridDescriptor
    !< - resolution must be the same
    !< - extends bounds to largest axis
    class(shr_gGridDescriptor), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
    class(shr_iGGridDescriptor), allocatable :: newGDescriptor !< output
    type(shr_gridBounds) :: combinedBounds

    if (self % getResolution() /= other % getResolution()) then
      call raiseError(__FILE__, "gridDescriptor_combine", &
              "Resolution from 'other' does not match 'self'")
    end if

    combinedBounds = self % getBounds() + other % getBounds()
    allocate(shr_gGridDescriptor :: newGDescriptor)
    call newGDescriptor % init(self % getResolution(), combinedBounds)
  end function gGridDescriptor_combine


  logical function gGridDescriptor_fitsIn(self, other)
    !< true if 'other' grid bounds fits in 'self'
    class(shr_gGridDescriptor), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: other
    type(shr_gGridDescriptor) :: gDesc

    select type(o => other)
    type is (shr_gGridDescriptor)
      gDesc = o
    class default !< expected type not found
      call raiseError(__FILE__, "gGridDescriptor_fitsIn", &
            "Unexpected type found")
    end select

    gGridDescriptor_fitsIn = self % bounds % fits(gDesc % getBounds())
  end function gGridDescriptor_fitsIn

end module shr_gGridDescriptor_mod 

