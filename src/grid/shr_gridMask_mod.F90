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
!> gridMask maps which gridcells are enabled or disabled
!>
!------------------------------------------------------------------------------
module shr_gridMask_mod
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_strings_mod, only: string
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
!  use shr_GridBounds_mod, only: shr_gridBounds
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

  implicit none

  public :: shr_gridMask, shr_IgridMask

  logical, parameter :: ISDEBUG = .false.


  !< interface to shr_gridMask
  type, abstract :: shr_IgridMask
  contains
    procedure(iface_getRaw), deferred :: getRaw
    procedure(iface_getGridDescriptor), deferred :: getGridDescriptor
  end type


  abstract interface
    function iface_getRaw(self) result (outMask)
      import :: shr_IgridMask
      !< returns current mask
      class(shr_IgridMask), intent(in) :: self
      logical, allocatable :: outMask(:,:) !< output
    end function iface_getRaw

    type(shr_gGridDescriptor) function iface_getGridDescriptor(self)
      import :: shr_IgridMask, shr_gGridDescriptor
      !< returns self gridDescriptor
      class(shr_IgridMask), intent(in) :: self
    end function iface_getGridDescriptor
  end interface


  type, extends(shr_IgridMask) :: shr_gridMask
    !< grid descriptor
    type(shr_gGridDescriptor), allocatable :: gridDescriptor

    logical, allocatable :: mask(:,:)
  contains
    procedure ::  gridMask_initialize
    procedure :: gridMask_initialize_by_larray
    generic :: init => gridMask_initialize, gridMask_initialize_by_larray

    procedure :: countEnabled
!    procedure :: setAll(status), assignment(=)

!    procedure :: setStatusByMask
    procedure :: setStatusByGridcellIndex
    generic :: setStatus => setStatusByGridcellIndex

!    procedure :: getStatusByMask
    procedure :: getStatusByGridcellIndex
    generic :: getStatus => getStatusByGridcellIndex

    procedure :: getRaw
    procedure :: getGridDescriptor

    procedure, pass(self) :: copy_rev_array
    procedure :: copy_gridMask
    generic :: assignment(=) => copy_gridMask, copy_rev_array

    procedure :: eq_scalar_logical
    procedure :: eq_rawMask
    procedure :: eq_gridMask
    generic :: operator(==) => eq_scalar_logical, eq_gridMask, &
            eq_rawMask

    !< same grid descriptor
    procedure :: or_bitwise
    generic :: operator(.or.) => or_bitwise
    procedure :: and_gridMask
    generic :: operator(.and.) => and_gridMask
    procedure :: reverse_gridMask_func
    procedure :: reverse => reverse_gridMask
    generic :: operator(.not.) => reverse_gridMask_func

    procedure, private :: isValidMask => isValidBy2dArray

    procedure :: any => any_gridMask

    procedure :: toString
  end type shr_gridMask

contains

  subroutine gridMask_initialize_by_larray(self, gridDescriptor, lmask)
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    type(shr_gGridDescriptor), intent(in) :: gridDescriptor
    logical, intent(in) :: lmask(:,:)

    call self % gridMask_initialize(gridDescriptor)
    self % mask = lmask
  end subroutine gridMask_initialize_by_larray



  subroutine gridMask_initialize(self, gridDescriptor, default)
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    type(shr_gGridDescriptor), intent(in) :: gridDescriptor
    logical, intent(in), optional :: default !< define default value (def: true)

    logical :: inDefault
    type(shr_gGridAxes) :: axis
    integer :: nlats, nlons

    inDefault = .true.
    if (present(default)) inDefault = default
    allocate(self % gridDescriptor, source = gridDescriptor) 

    axis = self % gridDescriptor % getLatAxis() !latitudes % getSize()
    nlats = axis % getSize()
    axis = self % gridDescriptor % getLonAxis() !longitudes % getSize()
    nlons = axis % getSize()

    allocate(self% mask(nlats, nlons))
    self % mask = inDefault
  end subroutine gridMask_initialize


  subroutine setStatusByGridcellIndex(self, gcIndex, newStatus)
    !< set a new status for given coordinate 'coord'
    !< coord must be found inside the grid
    class(shr_gridMask), intent(inout) :: self
    type(shr_gridcellIndex), intent(in) :: gcIndex 
    logical, intent(in) :: newStatus
    self % mask(gcIndex % idxlat, gcIndex % idxlon) = newStatus
  end subroutine setStatusByGridcellIndex


  logical function getStatusByGridcellIndex(self, gcIndex)
    !< returns the gridcell status requested by 'coord'
    class(shr_gridMask), intent(in) :: self
    type(shr_gridcellIndex), intent(in) :: gcIndex
    getStatusByGridcellIndex = self % mask(gcIndex % idxlat, gcIndex % idxlon)
  end function getStatusByGridcellIndex


  integer function countEnabled(self)
    !< return the number of enabled grid cells found
    class(shr_gridMask), intent(in) :: self
    countEnabled = count(self % mask) 
  end function countEnabled


  function getRaw(self) result (outMask)
    !< returns current mask
    class(shr_gridMask), intent(in) :: self
    logical, allocatable :: outMask(:,:) !< output
    allocate(outMask, source = self % mask)
  end function getRaw


  logical function eq_scalar_logical(self, value)
    !< true if all values match 'value'
    class(shr_gridMask), intent(in) :: self
    logical, intent(in) :: value
    eq_scalar_logical = all(self % mask .eqv. value)
  end function eq_scalar_logical


  logical function eq_rawMask(self, mask)
    !< true if all values match 'value'
    class(shr_gridMask), intent(in) :: self
    logical, intent(in) :: mask(:,:)
    character(*), parameter :: SNAME = "eq_rawMask"
    logical :: hasSameMask

    if (.not. self % isValidMask(mask)) then
      call raiseError(__FILE__, SNAME, &
              "Given 'mask' dimensions do not match the masked grid")
    end if
    hasSameMask = all(self % mask .eqv. mask)
    eq_rawMask = (hasSameMask)
  end function eq_rawMask


  logical function eq_gridMask(self, other)
    !< true if all values match 'value'
    class(shr_gridMask), intent(in) :: self
    type(shr_gridMask), intent(in) :: other
    logical :: hasSameDescriptor, hasSameMask
    hasSameDescriptor = (self % gridDescriptor == other % gridDescriptor)
    hasSameMask = all(self % mask .eqv. other % mask)
    eq_gridMask = (hasSameDescriptor .and. hasSameMask)
  end function eq_gridMask


  type(shr_gridMask) function and_gridMask(self, other) result (newMask)
    !< returns a new gridMask with matching gridcells
    !< both must have the same size
    !<
    !< gridMask(F T F) = gridMask(T T F) .and. gridMask(F T F)
    class(shr_gridMask), intent(in) :: self
    type(shr_gridMask), intent(in) :: other
    call newMask % init(self % gridDescriptor)
    newMask % mask = (self % mask .and. other % mask)
  end function and_gridMask


  subroutine reverse_gridMask(self)
    !< modify mask bitwise complement (.not.)
    class(shr_gridMask), intent(inout) :: self
    self % mask = .not. (self % mask)
  end subroutine reverse_gridMask


  type(shr_gridMask) function reverse_gridMask_func(self) result (newGM)
    !< bitwise complement (.not.)
    class(shr_gridMask), intent(in) :: self
    call newGM % init(self % getGridDescriptor())
    newGM % mask = .not. (self % mask)
  end function reverse_gridMask_func


  subroutine copy_gridMask(self, other)
    !< copy all attributes from 'other' to 'self'
    class(shr_gridMask), intent(inout) :: self
    class(shr_gridMask), intent(in) :: other
    if (allocated(self % mask)) deallocate(self % mask)
    if (allocated(self % gridDescriptor)) deallocate(self % gridDescriptor)
    allocate(self % mask, source = other % mask)
    allocate(self % gridDescriptor, source = other % gridDescriptor)
  end subroutine copy_gridMask


  subroutine copy_rev_array(mask, self)
    !< copy 'self' array mask into 'other'
    !< 'other' must be initializsed already
    !< 'other' gridDescriptor must match new array mask dimensions
    logical, intent(inout) :: mask(:,:)
    class(shr_gridMask), intent(in) :: self
    character(*), parameter :: SNAME = "copy_rev_array"

    if (.not. self % isValidMask(mask)) then
      call raiseError(__FILE__, SNAME, &
              "Given 'mask' dimensions do not match the masked grid")
    end if
    mask = self % mask
  end subroutine copy_rev_array


  logical function isValidBy2dArray(self, mask)
    !< true if given 2d array mask matches 'self' grid descriptor
    !< It matches when:
    !< - same dimensions for lat and lon
    !< todo: move to gridDescriptor
    class(shr_gridMask), intent(in) :: self
    logical, intent(in) :: mask(:,:)

    type(shr_gGridAxes) :: tmpGAxis
    integer :: nlats, nlons

    tmpGAxis = self % gridDescriptor % getLatAxis()
    nlats = tmpGAxis % getSize()

    tmpGAxis = self % gridDescriptor % getLonAxis()
    nlons = tmpGAxis % getSize()

    if (size(mask, dim=1) /= nlats) then
      isValidBy2dArray = .false.
      return
    end if

    if (size(mask, dim=2) /= nlons) then
      isValidBy2dArray = .false.
      return
    end if
    isValidBy2dArray = .true.
  end function isValidBy2dArray


  logical function any_gridMask(self)
    !< true if any 'a' and 'b' has true value
    !< wrap to enable 'any' from shr_gridMask
    class(shr_gridMask), intent(in) :: self
    any_gridMask = any(self % mask)
  end function any_gridMask


  type(shr_gGridDescriptor) function getGridDescriptor(self)
    !< returns self gridDescriptor
    class(shr_gridMask), intent(in) :: self
    getGridDescriptor = self % gridDescriptor
  end function getGridDescriptor


  type(shr_gridMask) function or_bitwise(self, other) result (newGMask)
    !< apply 'mask' into 'self' with the bitwise 'or' operation
    !< given 'mask' must have the same descriptor as 'self'
    class(shr_gridMask), intent(in) :: self
    type(shr_gridMask), intent(in) :: other
    !< enfore same resolution and grid position
    if (.not. self % gridDescriptor == other % getGridDescriptor()) then
      call raiseError(__FILE__, "or_bitwise", &
          "'mask' must have the same grid descriptor as 'self'")
    end if

    call newGMask % init(self % getGridDescriptor())
    newGMask % mask = (self % mask .or. other % mask)
  end function or_bitwise


  type(string) function toString(self)
    !< mask to string type
    !<
    !< 'T T'
    !< 'T T'
    !< 'F T'
    !<
    class(shr_gridMask), intent(in) :: self
    integer :: nlats, ilat
    type(shr_gGridAxes) :: latAxis
    character(:), allocatable :: tmp
    character(500) :: t
    latAxis = self % gridDescriptor % getLatAxis()
    nlats = latAxis % getSize()
    tmp = "" ! initialize
    do ilat = 1, nlats-1
      write(t, *) self % mask(ilat, :)
      tmp = tmp // "'" // trim(adjustl(t)) // "'" // NEW_LINE('a')
    end do
    write(t, *) self % mask(nlats, :)
    tmp = tmp // "'" // trim(adjustl(t)) // "'"
    toString = string(tmp)
  end function toString

end module shr_gridMask_mod

