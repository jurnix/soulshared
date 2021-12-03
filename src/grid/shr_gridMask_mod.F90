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

  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
!  use shr_GridBounds_mod, only: shr_gridBounds
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

  implicit none

  public :: shr_gridMask

  logical, parameter :: ISDEBUG = .false.


  type shr_gridMask
    !< grid descriptor
    type(shr_gGridDescriptor), allocatable :: gridDescriptor

    logical, allocatable :: mask(:,:)
  contains
    procedure :: init => gridMask_initialize

    procedure :: countEnabled
!    procedure :: setAll(status), assignment(=)

!    procedure :: setStatusByMask
    procedure :: setStatusByGridcellIndex
    generic :: setStatus => setStatusByGridcellIndex

!    procedure :: getStatusByMask
    procedure :: getStatusByGridcellIndex
    generic :: getStatus => getStatusByGridcellIndex

    procedure :: getRaw

    procedure, pass(self) :: copy_rev_array
    procedure :: copy_gridMask
    generic :: assignment(=) => copy_gridMask, copy_rev_array

    procedure :: eq_scalar_logical
    procedure :: eq_rawMask
    procedure :: eq_gridMask
    generic :: operator(==) => eq_scalar_logical, eq_gridMask, &
                eq_rawMask

    procedure :: and_gridMask
    generic :: operator(.and.) => and_gridMask

    procedure :: reverse => reverse_gridMask

    procedure, private :: isValidMask => isValidBy2dArray
  end type shr_gridMask

contains

  subroutine gridMask_initialize(self, gridDescriptor) 
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    type(shr_gGridDescriptor), intent(in) :: gridDescriptor 

    type(shr_gGridAxes) :: axis
    integer :: nlats, nlons

    allocate(self % gridDescriptor, source = gridDescriptor) 

    axis = self % gridDescriptor % getLatAxis() !latitudes % getSize()
    nlats = axis % getSize()
    axis = self % gridDescriptor % getLonAxis() !longitudes % getSize()
    nlons = axis % getSize()

    allocate(self% mask(nlats, nlons))
    self % mask = .true.
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

end module shr_gridMask_mod

