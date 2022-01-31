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
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_strings_mod, only: string, real2string

  implicit none

  public :: shr_gGridDescriptor, shr_iGGridDescriptor

  logical, parameter :: ISDEBUG = .false.


  !< interface
  type, abstract :: shr_iGGridDescriptor
  contains
    procedure(iface_initialize), deferred :: initialize
    procedure(iface_initialize_simple), deferred :: initialize_simple
    generic :: init => initialize, initialize_simple

    procedure(iface_fitsIn), deferred :: fitsIn
    procedure(iface_getResolution), deferred :: getResolution
    procedure(iface_getBounds), deferred :: getBounds

    procedure(iface_combine), deferred :: combine
    generic :: operator(+) => combine

    procedure(iface_equal), deferred :: equal
    generic :: operator(==) => equal

    procedure(iface_toString), deferred :: toString
  end type shr_iGGridDescriptor


  abstract interface
    elemental subroutine iface_initialize(self, resolution, bounds, latAxis, lonAxis)
      import :: shr_iGGridDescriptor, sp, shr_gridBounds, shr_gAxis
      !< grid descriptor initialization
      class(shr_iGGridDescriptor), intent(inout) :: self
      real(kind=sp), intent(in) :: resolution
      type(shr_gridBounds), intent(in) :: bounds
      type(shr_gAxis), intent(in) :: latAxis
      type(shr_gAxis), intent(in) :: lonAxis
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

    elemental type(shr_gAxis) function iface_getLatAxis(self)
      import :: shr_iGGridDescriptor, shr_gAxis
      !< return class bounds
      class(shr_iGGridDescriptor), intent(in) :: self
    end function iface_getLatAxis

    elemental type(shr_gAxis) function iface_getLonAxis(self)
      import :: shr_iGGridDescriptor, shr_gAxis
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

    elemental impure type(shr_gridBounds) function iface_getBounds(self)
      import :: shr_iGGridDescriptor, shr_gridBounds
      !< return class bounds
      class(shr_iGGridDescriptor), intent(in) :: self
    end function iface_getBounds

    elemental impure logical function iface_equal(self, other)
      import :: shr_iGGridDescriptor
      !< ==
      class(shr_iGGridDescriptor), intent(in) :: self
      class(shr_iGGridDescriptor), intent(in) :: other
    end function iface_equal

    type(string) function iface_toString(self)
      import :: shr_igGridDescriptor, string
      !<
      class(shr_iGGridDescriptor), intent(in) :: self
    end function iface_toString
  end interface


  type, extends(shr_iGGridDescriptor) :: shr_gGridDescriptor
    real(kind=sp) :: resolution = -1
    type(shr_gridBounds), allocatable :: bounds
    type(shr_gAxis), allocatable :: latAxis
    type(shr_gAxis), allocatable :: lonAxis
  contains
    procedure :: initialize => gGridDescriptor_initialize
    procedure :: initialize_simple => gGridDescriptor_initialize_simple
    !generic :: init => gGridDescriptor_initialize, gGridDescriptor_initialize_simple

    procedure :: getResolution => gGridDescriptor_getResolution
    procedure :: getBounds => gGridDescriptor_getBounds

    procedure :: equal => gGridDescriptor_equal
    !generic :: operator(==) => eq_gridDescriptor

    procedure :: combine => gGridDescriptor_combine
    !generic :: operator(+) => gridDescriptor_combine

    procedure :: fitsIn => gGridDescriptor_fitsIn
    procedure :: toString
  end type shr_gGridDescriptor

contains


  elemental subroutine gGridDescriptor_initialize(self, resolution, bounds, latAxis, lonAxis)
    !< grid descriptor initialization
    class(shr_gGridDescriptor), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gridBounds), intent(in) :: bounds
    type(shr_gAxis), intent(in) :: latAxis
    type(shr_gAxis), intent(in) :: lonAxis
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

    !write(*,*) "gGridDescriptor_mod:: gGridDescriptor_initialize_simple :: n, s =", bounds % north, bounds % south
    !write(*,*) "gGridDescriptor_mod:: gGridDescriptor_initialize_simple :: e, w =", bounds % east, bounds % west

    call latAxisBounds % init(bounds % north, bounds % south)
    call lonAxisBounds % init(bounds % east, bounds % west)

    latName = string("latitude")
    allocate(self % latAxis)
    call self % latAxis % init(latName, resolution, latAxisBounds)
    !write(*,*) "gGridDescriptor_mod:: gGridDescriptor_initialize_simple :: lats size =", self % latAxis % getSize()

    lonName = string("longitude")
    allocate(self % lonAxis)
    call self % lonAxis % init(lonName, resolution, lonAxisBounds)
    !write(*,*) "gGridDescriptor_mod:: gGridDescriptor_initialize_simple :: lons size =", self % lonAxis % getSize()
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


  elemental impure logical function gGridDescriptor_equal(self, other)
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
    gGridDescriptor_fitsIn = self % bounds % fits(other % getBounds())
  end function gGridDescriptor_fitsIn


  type(string) function toString(self)
    !< string representation of gGridDescriptor
    class(shr_gGridDescriptor), intent(in) :: self
    type(string) :: resStr, boundsStr
    resStr = real2string(self % getResolution())
    boundsStr = self % bounds % toString()
    toString = string("resolution= ")
    toString = toString + resStr + ", bounds=(" + boundsStr + ")"
  end function toString

end module shr_gGridDescriptor_mod 

