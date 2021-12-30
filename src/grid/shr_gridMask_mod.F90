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
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_coord_mod, only: shr_coord
  use shr_gridIndicesMapping_mod, only: shr_gridIndicesMapping

  use shr_arrayIndices_mod, only: shr_arrayIndices
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices

  implicit none

  public :: shr_gridMask, shr_IgridMask, shr_gridMask_cast

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

    !procedure :: get
    procedure :: set
    procedure, private :: findIndices
    procedure :: select
    procedure :: expand
    procedure :: toString

    procedure :: isIncluded
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


  subroutine shr_gridMask_cast(obj, gMask)
    !< Cast from * to shr_gridMask
    class(*), intent(in) :: obj
    type(shr_gridMask), intent(out) :: gMask

    select type(o => obj)
    type is(shr_gridMask)
      gMask = o
    class default
      call raiseError(__FILE__, &
          "shr_gridMask_cast", &
          "Unexpected type found instead of 'shr_gridMask'")
    end select

  end subroutine shr_gridMask_cast


  type(shr_gridMask) function select(self, gDescriptor) result (newGMask)
    !< select a new shr_gridMask according to gDescriptor
    !< new gDscriptor must fit self % gridDescriptor
    class(shr_gridMask), intent(in) :: self
    type(shr_gGridDescriptor), intent(in) :: gDescriptor

    logical, allocatable :: newLmask(:,:)
    type(shr_gridIndicesMapping) :: idxMapping
    type(shr_gridBoundIndices) :: gBoundIndices

    if (.not. self % gridDescriptor % fitsIn(gDescriptor) ) then
      call raiseError(__FILE__, "select", &
          "gDescriptor argument does not fit in the current shr_gridDomain")
    end if

    !< discover array indices
    call idxMapping % init(gDescriptor)
    gBoundIndices = self % findIndices(gDescriptor, idxMapping)

    call newGMask % init(gDescriptor, default = .false.)!, newLmask)
    call newGMask % set(self % mask, gBoundIndices)
  end function select


  type(shr_gridBoundIndices) function findIndices(self, gDescriptor, idxMapping) result (gBoundsIndices)
    !<
    class(shr_gridMask), intent(in) :: self
    type(shr_gGridDescriptor), intent(in) :: gDescriptor
    type(shr_gridIndicesMapping), intent(in) :: idxMapping

    real(kind=sp) :: halfres
    type(shr_gridBounds) :: bounds
    type(shr_coord) :: cTopLeft, cBottomRight
    type(shr_gridcellIndex), allocatable :: gIndicesTL(:), gIndicesBR(:)
    integer :: startlat, endlat
    integer :: startlon, endlon

    !< from bounds get top-left and bottom-right coordinates
    !< center of coordinate (it enforeces to select a unique gIndices)
    halfRes = gDescriptor % getResolution() / 2.
    bounds = gDescriptor % getBounds()
    ! todo: change east vs west, wrong
    cTopLeft = shr_coord( bounds % getNorth() - halfRes, bounds % getEast() - halfres)
    cBottomRight = shr_coord( bounds % getSouth() + halfres, bounds % getWest() + halfres)

    !< discover array indices
    !< find array indices from top-left
    gIndicesTL = idxMapping % getIndex(cTopLeft) !< only 1
    !< find array indices from bottom-right
    gIndicesBR = idxMapping % getIndex(cBottomRight) !< only 1

    startlat = gIndicesTL(1) % idxLat
    endlat = gIndicesBR(1) % idxLat
    startlon = gIndicesTL(1) % idxLon
    endlon =  gIndicesBR(1) % idxLon
    call gBoundsIndices % init(startlat, endlat, startlon, endlon)
  end function findIndices


  type(shr_gridMask) function expand(self, gDescriptor) result (newGMask)
    !< returns a new shr_gridMask with an expanded grid
    !< - 'self' must fit into 'gDescriptor'
    !< - mask remains the same
    class(shr_gridMask), intent(in) :: self
    !type(shr_gridBounds), intent(in) :: bounds
    type(shr_gGridDescriptor), intent(in) :: gDescriptor

    type(shr_gridBoundIndices) :: gBoundIndices
    logical, allocatable :: newLMask(:,:)
    type(shr_gridIndicesMapping) :: idxMapping

    !< new bounds fit in current gridMask?
    if (.not. gDescriptor % fitsIn(self % gridDescriptor) ) then
      call raiseError(__FILE__, "expand", &
          "gDescriptor argument does not fit in the current shr_gridDomain")
    end if

    !< temporary mask to initialze mask with proper dimensions
    call newGMask % init(gDescriptor)
    !< find indices from new big to small
    call idxMapping % init(self % gridDescriptor)
    gBoundIndices = newGMask % findIndices(self % gridDescriptor, idxMapping)

    call newGMask % set(self % mask, gBoundIndices)
  end function expand


  subroutine set(self, mask, gBindices)
    !< set values of mask into self
    !< when defined gBindices:
    !< - place 'mask' into gBIndices indices
    !<
    !< 'mask' shape must be consistent with 'self'
    !< 'gBindices' must be consistent with 'mask' and 'self'
    class(shr_gridMask), intent(inout) :: self
    logical, intent(in) :: mask(:,:)
    type(shr_gridBoundIndices), intent(in), optional :: gBindices
    type(shr_gGridDescriptor) :: newGDescriptor

    integer :: startCol, endCol
    integer :: startRow, endRow
    integer :: nrows, ncols

    !< same shapes?
    if (.not. present(gBIndices)) then
      if (any(shape(mask) /= shape(self % mask))) then
        call raiseError(__FILE__, "set", &
            "mask and self % mask shape do not conform")
      end if
    end if

    !< default
    startRow = 1
    endRow = size(self % mask, dim=1)
    startCol = 1
    endCol = size(self % mask, dim=2)
    nrows = endRow - startRow + 1
    ncols = endRow - startCol + 1
    if (present(gBindices)) then

      startRow = gBindices % getStartRow()
      endRow =  gBindices % getEndRow()
      startCol = gBindices %  getStartCol()
      endCol =  gBindices %  getEndCol()
      ncols = gBindices % countCols()
      nrows = gBindices % countRows()

      !write(*,*) "set:: argument found!"
      !write(*,*) "set:: lat (start, end) =", gBindices % getStartRow(), gBindices % getEndRow()
      !write(*,*) "set:: lon (start, end) =", gBindices %  getStartCol(), gBindices %  getEndCol()
    end if

    !< proper dimensions?
    !< todo: switch east vs west
    !write(*,*) "set:: cols (start, end) =", startCol, endCol
    !write(*,*) "set:: rows (start, end) =", startRow, endRow
    !write(*,*) "set:: self % mask shape? ", shape(self % mask)
    !write(*,*) "set:: ncols, nrows = ", ncols, nrows
    !write(*,*) "set:: given mask shape? ", shape(mask)
    self % mask(startRow:endRow, startCol:endCol) = mask(1:nrows,1:ncols)
  end subroutine set


  logical function isIncluded(self, other)
    !< true if other gridMask true gridcells also match self mask array
    !<
    !< self (TT,FF) % isMaskIncluded(TF,FF) -> true
    !< self (TT,FF) % isMaskIncluded(TF,FT) -> false
    !<
    !< 'self' matches with 'other' gridcells?
    !< ('self' gridcells are disabled in 'other')
    !< partition self
    !< T T F F F T -> True where border
    !<
    !< land grid cells -> True where land grid cells
    !< F F T F T F   -> rev (select disabled) -> T T F T F T
    !<                                              .and.
    !<                                 (border)  T T - - - T
    !< all true? -> yes
    class(shr_gridMask), intent(in) :: self
    type(shr_gridMask), intent(in) :: other

    type(shr_gridMask) :: reversedMask, disabledMask

    reversedMask = other
    !< select potential border cells
    call reversedMask % reverse()
    !< potential border cells match with chosen 'border'?
    disabledMask = (self .and. reversedMask)
    !< all match?
    isIncluded = (disabledMask == other)
  end function isIncluded

end module shr_gridMask_mod

