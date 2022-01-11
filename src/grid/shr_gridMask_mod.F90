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

  use shr_strings_mod, only: string, int2string, stringCollection
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
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
    procedure(iface_gridMask_initialize_by_larray), deferred :: initialize_by_larray
    procedure(iface_gridMask_initialize), deferred :: initialize
    generic :: init => initialize_by_larray, initialize

    procedure(iface_getRaw), deferred :: getRaw
    procedure(iface_getGridDescriptor), deferred :: getGridDescriptor
    !procedure(iface_isincluded), deferred :: isIncluded

    procedure(iface_equal_scalar_logical), deferred :: equal_scalar_logical
    procedure(iface_equal_rawMask), deferred :: equal_rawMask
    procedure(iface_equal_gridMask), deferred :: equal_gridMask
    generic :: operator(==) => equal_scalar_logical, equal_gridMask, &
        equal_rawMask

    procedure(iface_and_gridMask), deferred :: and_gridMask
    generic :: operator(.and.) => and_gridMask
    procedure(iface_any), deferred :: any

    procedure(iface_expand), deferred :: expand
    procedure(iface_select), deferred :: select
    procedure(iface_set), deferred :: set

    procedure(iface_toString), deferred :: toString
    procedure(iface_getShape), deferred :: getShape
    procedure, private :: findIndices
  end type


  abstract interface
    subroutine iface_gridMask_initialize_by_larray(self, gridDescriptor, lmask)
      import :: shr_igridMask, shr_iGGridDescriptor
      !< gridMask initialization
      class(shr_igridMask), intent(inout) :: self
      class(shr_iGGridDescriptor), intent(in) :: gridDescriptor
      logical, intent(in) :: lmask(:,:)
    end subroutine iface_gridMask_initialize_by_larray

    subroutine iface_gridMask_initialize(self, gridDescriptor, default)
      import :: shr_igridMask, shr_iGGridDescriptor
      !< gridMask initialization
      class(shr_igridMask), intent(inout) :: self
      class(shr_iGGridDescriptor), intent(in) :: gridDescriptor
      logical, intent(in), optional :: default !< define default value (def: true)
    end subroutine iface_gridMask_initialize

    function iface_getRaw(self, gBoundIndices) result (newMask)
      import :: shr_IgridMask, shr_gridBoundIndices
      !< returns current mask
      !< in case gBoundIndices are defined it returns the selected indices
      !< those indices must be valid
      class(shr_IgridMask), intent(in) :: self
      type(shr_gridBoundIndices), intent(in), optional :: gBoundIndices
      logical, allocatable :: newMask(:,:) !< output
    end function iface_getRaw

    function iface_getGridDescriptor(self) result(newGDescriptor)
      import :: shr_IgridMask, shr_iGGridDescriptor
      !< returns self gridDescriptor
      class(shr_IgridMask), intent(in) :: self
      class(shr_iGGridDescriptor), allocatable :: newGDescriptor !< output
    end function iface_getGridDescriptor

    !logical function iface_isIncluded(self, other)
    !  import :: shr_igridMask
      !< true if other gridMask true gridcells also match self mask array
    !  class(shr_igridMask), intent(in) :: self
    !  class(shr_igridMask), intent(in) :: other
    !end function iface_isIncluded

    logical function iface_equal_scalar_logical(self, value)
      import :: shr_igridMask
      !< true if all values match 'value'
      class(shr_igridMask), intent(in) :: self
      logical, intent(in) :: value
    end function iface_equal_scalar_logical

    logical function iface_equal_rawMask(self, mask)
      import :: shr_igridMask
      !< true if all values match 'value'
      class(shr_igridMask), intent(in) :: self
      logical, intent(in) :: mask(:,:)
    end function iface_equal_rawMask

    logical function iface_equal_gridMask(self, other)
      import :: shr_igridMask
      !< true if all values match 'value'
      class(shr_igridMask), intent(in) :: self
      class(shr_igridMask), intent(in) :: other
    end function iface_equal_gridMask

    function iface_expand(self, gDescriptor) result (newGMask)
      import :: shr_igridMask, shr_iGGridDescriptor
      !< returns a new shr_gridMask with an expanded grid
      !< - 'self' must fit into 'gDescriptor'
      !< - mask remains the same
      class(shr_igridMask), intent(in) :: self
      !type(shr_gridBounds), intent(in) :: bounds
      class(shr_iGGridDescriptor), intent(in) :: gDescriptor
      class(shr_igridMask), allocatable :: newGMask !< output
    end function iface_expand

    function iface_select(self, gDescriptor) result (newGMask)
      import :: shr_igridMask, shr_iGGridDescriptor
      !< select a new shr_gridMask according to gDescriptor
      !< new gDscriptor must fit self % gridDescriptor
      class(shr_igridMask), intent(in) :: self
      class(shr_iGGridDescriptor), intent(in) :: gDescriptor
      class(shr_igridMask), allocatable :: newGMask !< output
    end function iface_select

    function iface_and_gridMask(self, other) result (newMask)
      import :: shr_igridMask
      !< returns a new gridMask with matching gridcells
      !< both must have the same size
      !<
      !< gridMask(F T F) = gridMask(T T F) .and. gridMask(F T F)
      class(shr_igridMask), intent(in) :: self
      class(shr_igridMask), intent(in) :: other
      class(shr_igridMask), allocatable :: newMask !< output
    end function iface_and_gridMask

    subroutine iface_set(self, mask, gBindices)
      import :: shr_igridMask, shr_gridBoundIndices
      !< set values of mask into self
      !< when defined gBindices:
      !< - place 'mask' into gBIndices indices
      !<
      !< 'mask' shape must be consistent with 'self'
      !< 'gBindices' must be consistent with 'mask' and 'self'
      class(shr_igridMask), intent(inout) :: self
      logical, intent(in) :: mask(:,:)
      type(shr_gridBoundIndices), intent(in), optional :: gBindices
    end subroutine iface_set

    logical function iface_any(self)
      import :: shr_igridMask
      !< true if any 'a' and 'b' has true value
      !< wrap to enable 'any' from shr_gridMask
      class(shr_igridMask), intent(in) :: self
    end function iface_any

    type(string) function iface_toString(self)
      import :: shr_igridMask, string
      !< mask to string type
      class(shr_igridMask), intent(in) :: self
    end function iface_toString

   function iface_getShape(self) result (shape)
      import :: shr_igridMask
      !< grid shape
      class(shr_igridMask), intent(in) :: self
      integer :: shape(2)
    end function iface_getShape
  end interface


  type, extends(shr_IgridMask) :: shr_gridMask
    !< grid descriptor
    class(shr_iGGridDescriptor), allocatable :: gridDescriptor

    logical, allocatable :: mask(:,:)
  contains
    procedure :: initialize => gridMask_initialize
    procedure :: initialize_by_larray => gridMask_initialize_by_larray

    procedure :: countEnabled
!    procedure :: setAll(status), assignment(=)

!    procedure :: setStatusByMask
    procedure :: setStatusByGridcellIndex
    generic :: setStatus => setStatusByGridcellIndex

!    procedure :: getStatusByMask
    procedure :: getStatusByGridcellIndex
    generic :: getStatus => getStatusByGridcellIndex

    procedure :: getRaw
    procedure :: getGridDescriptor => gridMask_getGridDescriptor

    procedure, pass(self) :: copy_rev_array
    procedure :: copy_gridMask
    generic :: assignment(=) => copy_gridMask, copy_rev_array

    procedure :: equal_scalar_logical => gridMask_equal_scalar_logical
    procedure :: equal_rawMask => gridMask_equal_rawMask
    procedure :: equal_gridMask => gridMask_equal_gridMask

    !< same grid descriptor
    procedure :: or_bitwise
    generic :: operator(.or.) => or_bitwise
    procedure :: and_gridMask => gridMask_and_gridMask

    procedure :: reverse_gridMask_func
    procedure :: reverse => reverse_gridMask
    generic :: operator(.not.) => reverse_gridMask_func

    procedure, private :: isValidMask => isValidBy2dArray

    procedure :: any => any_gridMask

    !procedure :: get
    procedure :: set => gridMask_set
    procedure :: select => gridMask_select
    procedure :: expand => gridMask_expand
    procedure :: toString => gridMask_toString
    procedure :: getShape => gridMask_getShape

    !procedure :: isIncluded => gridMask_isIncluded
  end type shr_gridMask

contains

  subroutine gridMask_initialize_by_larray(self, gridDescriptor, lmask)
    !< gridMask initialization
    !< lmask shape must be consistent with gridDescriptor
    class(shr_gridMask), intent(inout) :: self
    class(shr_iGGridDescriptor), intent(in) :: gridDescriptor
    logical, intent(in) :: lmask(:,:)

    integer :: lshape(2), inMaskShape(2)
    type(stringCollection) :: tmpCollection, tmpCollection1
    type(string), allocatable :: tmp(:), tmp1(:)

    call self % initialize(gridDescriptor)

    !< lmask shape matches gridDescriptor shape?
    lshape = self % getShape()
    inMaskShape(1) = size(lmask,dim=1)
    inMaskShape(2) = size(lmask,dim=2)
    if (any(lshape /= inMaskShape)) then
      tmp = int2string(lshape)
      tmpCollection = stringCollection(tmp)
      tmp1 = int2string(inMaskShape)
      tmpCollection1 = stringCollection(tmp1)
      call raiseError(__FILE__, "gridMask_initialize_by_larray", &
          "Expected mask shape: "//tmpCollection % toString(), &
          "But found: "//tmpCollection1 % toString())
    end if
    self % mask = lmask
  end subroutine gridMask_initialize_by_larray


  subroutine gridMask_initialize(self, gridDescriptor, default)
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    class(shr_iGGridDescriptor), intent(in) :: gridDescriptor
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
    write(*,*) "gridMask_mod:: gridMask_initialize:: nlats (dim=1) = ", nlats
    write(*,*) "gridMask_mod:: gridMask_initialize:: nlons (dim=2) = ", nlons
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


  function getRaw(self, gBoundIndices) result (newMask)
    !< returns current mask
    !< todo: change to get(...)
    class(shr_gridMask), intent(in) :: self
    type(shr_gridBoundIndices), intent(in), optional :: gBoundIndices
    logical, allocatable :: newMask(:,:) !< output
    integer :: ncols, nrows
    integer :: startRow, endRow
    integer :: startCol, endCol
    if (present(gBoundIndices)) then
      ncols = gBoundIndices % countCols()
      nrows = gBoundIndices % countRows()

      startCol = gBoundIndices % getStartCol()
      endCol = gBoundIndices % getEndCol()
      startRow = gBoundIndices % getStartRow()
      endRow = gBoundIndices % getEndRow()
    else
      nrows = size(self % mask, dim=1)
      ncols = size(self % mask, dim=2)

      startCol = 1
      endCol = ncols
      startRow = 1
      endRow = nrows
    end if

    allocate(newMask(nrows, ncols))
    newMask(1:nrows, 1:ncols) = self % mask(startRow:endRow, startCol:endCol )
  end function getRaw



  logical function gridMask_equal_scalar_logical(self, value)
    !< true if all values match 'value'
    class(shr_gridMask), intent(in) :: self
    logical, intent(in) :: value
    gridMask_equal_scalar_logical = all(self % mask .eqv. value)
  end function gridMask_equal_scalar_logical


  logical function gridMask_equal_rawMask(self, mask)
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
    gridMask_equal_rawMask = (hasSameMask)
  end function gridMask_equal_rawMask


  logical function gridMask_equal_gridMask(self, other)
    !< true if all values match 'value'
    class(shr_gridMask), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
    logical :: hasSameDescriptor, hasSameMask
    type(shr_gridMask) :: ogMask

    select type (o => other)
    type is (shr_gridMask)
      ogMask = o
    class default
      gridMask_equal_gridMask = .false.
    end select

    hasSameDescriptor = (self % gridDescriptor == ogMask % gridDescriptor)
    hasSameMask = all(self % mask .eqv. ogMask % mask)
    gridMask_equal_gridMask = (hasSameDescriptor .and. hasSameMask)
  end function gridMask_equal_gridMask


  function gridMask_and_gridMask(self, other) result (newMask)
    !< returns a new gridMask with matching gridcells
    !< both must have the same size
    !<
    !< gridMask(F T F) = gridMask(T T F) .and. gridMask(F T F)
    class(shr_gridMask), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
    class(shr_igridMask), allocatable :: newMask !< output
    logical, allocatable :: lmask(:,:)
    !< copy shape
    allocate(lmask, mold = self % mask)
    lmask = (self % mask .and. other % getRaw() )

    allocate(shr_gridMask :: newMask)
    call newMask % init(self % gridDescriptor, lmask)
  end function gridMask_and_gridMask


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


  function gridMask_getGridDescriptor(self) result (newGDescriptor)
    !< returns self gridDescriptor
    class(shr_gridMask), intent(in) :: self
    class(shr_iGGridDescriptor), allocatable :: newGDescriptor !< output
    !newGDescriptor = self % gridDescriptor
    allocate(newGDescriptor, source = self % gridDescriptor)
  end function gridMask_getGridDescriptor


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


  type(string) function gridMask_toString(self)
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
    gridMask_toString = string(tmp)
  end function gridMask_toString


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


  function gridMask_select(self, gDescriptor) result (newGMask)
    !< select a new shr_gridMask according to gDescriptor
    !< new 'gDescriptor' must fit self % gridDescriptor
    !< the output gridMask will have 'gDescriptor' as grid descriptor
    !< and the mask values matching 'self'
    class(shr_gridMask), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: gDescriptor
    class(shr_igridMask), allocatable :: newGMask !< output

    logical, allocatable :: newLmask(:,:)
    type(shr_gridIndicesMapping) :: idxMapping
    type(shr_gridBoundIndices) :: gBoundIndices
    type(string) :: tmp
    logical, allocatable :: tmpMask(:,:)

    if (.not. self % gridDescriptor % fitsIn(gDescriptor) ) then
      call raiseError(__FILE__, "select", &
          "gDescriptor argument does not fit in the current shr_gridDomain")
    end if

    !< discover array indices
    call idxMapping % init(self % getGridDescriptor())
    gBoundIndices = self % findIndices(gDescriptor, idxMapping)
    write(*,*) "gridMAsk_mod:: gridMask_select:: gBoundIndices (n, s, e, w)=", &
        gBoundIndices % startRow, gBoundIndices % endRow, &
        gBoundIndices % startCol, gBoundIndices % endCol

    allocate(shr_gridMask :: newGMask)
    call newGMask % init(gDescriptor, default = .false.)!, newLmask)
    tmp = gDescriptor % toString()
    write(*,*) "gridMAsk_mod:: gridMask_select:: newGMask gDescriptor =", tmp % toString()
    !tmpMask = self % getRaw(gBoundIndices)
    !call newGMask % set(tmpMask)
    call newGMask % set(self % mask, gBoundIndices)
  end function gridMask_select


  type(shr_gridBoundIndices) function findIndices(self, gDescriptor, idxMapping) result (gBoundsIndices)
    !< discover array indices given from gDescriptor opposed to self
    !< gDescriptor: requested indices (must be included in self)
    !< idxMapping: calculate indices
    class(shr_igridMask), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: gDescriptor
    type(shr_gridIndicesMapping), intent(in) :: idxMapping

    real(kind=sp) :: halfres
    type(shr_gridBounds) :: bounds
    type(shr_coord) :: cTopLeft, cBottomRight
    type(shr_gridcellIndex), allocatable :: gIndicesTL(:), gIndicesBR(:)
    integer :: startlat, endlat
    integer :: startlon, endlon
    class(shr_iGGridDescriptor), allocatable :: currentSelfGDescriptor

    type(string) :: tmp
    currentSelfGDescriptor = self % getGridDescriptor()
    tmp = currentSelfGDescriptor % toString()
    write(*,*) "gridMask_mod:: findIndices:: current descriptor =", tmp % toString()
    tmp = gDescriptor % toString()
    write(*,*) "gridMask_mod:: findIndices:: (argument) gDescriptor =", tmp % toString()

    !< from bounds get top-left and bottom-right coordinates
    !< center of coordinate (it enforeces to select a unique gIndices)
    halfRes = gDescriptor % getResolution() / 2.
    bounds = gDescriptor % getBounds()
    ! todo: change east vs west, wrong
    cTopLeft = shr_coord( bounds % getNorth() - halfRes, bounds % getEast() - halfres)
    cBottomRight = shr_coord( bounds % getSouth() + halfres, bounds % getWest() + halfres)
    write(*,*) "gridMask_mod:: findIndices:: topLeft = ", &
        bounds % getNorth() - halfRes, &
        bounds % getEast() - halfres
    write(*,*) "gridMask_mod:: findIndices:: bottomright= ", &
        bounds % getSouth() + halfres, &
        bounds % getWest() + halfres

    !< discover array indices
    !< find array indices from top-left
    gIndicesTL = idxMapping % getIndex(cTopLeft) !< only 1
    write(*,*) "gridMask_mod:: findIndices:: top left indices found = ", size(gIndicesTL)
    !< find array indices from bottom-right
    gIndicesBR = idxMapping % getIndex(cBottomRight) !< only 1
    write(*,*) "gridMask_mod:: findIndices:: bottom right indices found = ", size(gIndicesBR)

    startlat = gIndicesTL(1) % idxLat
    endlat = gIndicesBR(1) % idxLat
    startlon = gIndicesTL(1) % idxLon
    endlon =  gIndicesBR(1) % idxLon
    call gBoundsIndices % init(startlat, endlat, startlon, endlon)
  end function findIndices


   function gridMask_expand(self, gDescriptor) result (newGMask)
    !< returns a new shr_gridMask with an expanded grid
    !< - 'self' must fit into 'gDescriptor'
    !< - mask remains the same
    class(shr_gridMask), intent(in) :: self
    !type(shr_gridBounds), intent(in) :: bounds
    class(shr_iGGridDescriptor), intent(in) :: gDescriptor
    class(shr_igridMask), allocatable :: newGMask

    type(shr_gridBoundIndices) :: gBoundIndices
    logical, allocatable :: newLMask(:,:)
    type(shr_gridIndicesMapping) :: idxMapping

    !< new bounds fit in current gridMask?
    if (.not. gDescriptor % fitsIn(self % gridDescriptor) ) then
      call raiseError(__FILE__, "expand", &
          "gDescriptor argument does not fit in the current shr_gridDomain")
    end if

    !< temporary mask to initialze mask with proper dimensions
    allocate(shr_gridMask :: newGMask)
    call newGMask % init(gDescriptor)
    !< find indices from new big to small
    call idxMapping % init(self % gridDescriptor)
    gBoundIndices = newGMask % findIndices(self % gridDescriptor, idxMapping)

    call newGMask % set(self % mask, gBoundIndices)
  end function gridMask_expand


  subroutine gridMask_set(self, mask, gBindices)
    !< set values of mask into self
    !< 'gBindices' indices refer to self % mask
    !< when defined gBindices:
    !< - place 'mask' into self using gBindices
    !<
    !< 'mask' shape must be consistent with 'self'
    !< - 'mask' shape must be the same as self%mask if gBindices not defined
    !< - 'mask' shape must be the same as gBindices if defined
    !<
    class(shr_gridMask), intent(inout) :: self
    logical, intent(in) :: mask(:,:)
    type(shr_gridBoundIndices), intent(in), optional :: gBindices

    integer :: startCol, endCol
    integer :: startRow, endRow
    integer :: nrows, ncols
    integer :: gbIndicesNRows, gbIndicesNCols

    !< same shapes?
    if (.not. present(gBIndices)) then
      if (any(shape(mask) /= shape(self % mask))) then
        write(*,*) "gridMask_mod:: set:: 'self % mask' shape (rows, cols)? ", shape(self % mask)
        write(*,*) "gridMask_mod:: set:: 'mask' shape (rows, cols)? ", shape(mask)
        call raiseError(__FILE__, "set", &
            "mask and self % mask shape do not conform")
      end if
    end if

    if (present(gBindices)) then
      !< gBindices shape must be the same as 'mask' argument
      gbIndicesNCols = gBindices % countCols()
      gbIndicesNRows = gBindices % countRows()
      !< same shape?
      if (any([gbIndicesNRows, gbIndicesNCols] /= shape(mask))) then
        write(*,*) "gridMask_mod:: set:: gbIndices (rows, cols)? ", gbIndicesNRows, gbIndicesNCols
        write(*,*) "gridMask_mod:: set:: 'mask' shape (rows, cols)? ", shape(mask)
        call raiseError(__FILE__, "set", &
            "gbIndices and mask argument shapes do not conform")
      end if
    end if

    !< default
    startRow = 1
    endRow = size(self % mask, dim=1)
    startCol = 1
    endCol = size(self % mask, dim=2)
    nrows = endRow - startRow + 1
    ncols = endCol - startCol + 1
    if (present(gBindices)) then

      startRow = gBindices % getStartRow()
      endRow =  gBindices % getEndRow()
      startCol = gBindices %  getStartCol()
      endCol =  gBindices %  getEndCol()
      ncols = gBindices % countCols()
      nrows = gBindices % countRows()

      write(*,*) "set:: argument found!"
      !write(*,*) "set:: lat (start, end) =", gBindices % getStartRow(), gBindices % getEndRow()
      !write(*,*) "set:: lon (start, end) =", gBindices %  getStartCol(), gBindices %  getEndCol()
    end if

    !< proper dimensions?
    !< todo: switch east vs west

    write(*,*) "gridMask_mod:: set:: self % mask shape (rows, cols)? ", shape(self % mask)
    write(*,*) "gridMask_mod:: set:: self % mask args, rows (start, end) =", startRow, endRow
    write(*,*) "gridMask_mod:: set:: self % mask args, cols (start, end) =", startCol, endCol
    write(*,*) "gridMask_mod:: set:: given mask shape? ", shape(mask)
    write(*,*) "gridMask_mod:: set:: given mask args, nrows, ncols = 1:", nrows, ", 1:", ncols

    write(*,*) "gridMask_mod:: set:: self % mask(",startRow,":",endRow,&
                ",",startCol,":",endCol,&
                ") = mask(1:",nrows,",1:",ncols,")"
    self % mask(startRow:endRow, startCol:endCol) = mask(1:nrows,1:ncols)
  end subroutine gridMask_set


  !logical function gridMask_isIncluded(self, other)
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
  !  class(shr_gridMask), intent(in) :: self
  !  class(shr_igridMask), intent(in) :: other

  !  type(shr_gridMask) :: reversedMask, disabledMask
  !  type(string) :: tmp

  !  select type (o => other)
  !  type is (shr_gridMask)
  !    reversedMask = o
  !  class default
  !    call raiseError(__FILE__, &
  !        "gridMask_isIncluded", &
  !        "Unexpected type found instead of 'shr_gridMask'")
  !  end select
  !  tmp = self % toString()
  !  write(*,*) "gridMask_mod:: gridMask_isIncluded:: self =", tmp % toString()
  !  tmp = reversedMask % toString()
  !  write(*,*) "gridMask_mod:: gridMask_isIncluded:: other =", tmp % toString()

    !< select potential border cells
  !  call reversedMask % reverse()
  !  tmp = reversedMask % toString()
  !  write(*,*) "gridMask_mod:: gridMask_isIncluded:: other % reversedMask =", tmp % toString()
    !< potential border cells match with chosen 'border'?
  !  disabledMask = (self .and. reversedMask)
  !  tmp = disabledMask % toString()
  !  write(*,*) "gridMask_mod:: gridMask_isIncluded:: (self and reversed) disabledMask =", tmp % toString()
    !< all match?
  !  tmp = other % toString()
  !  write(*,*) "gridMask_mod:: gridMask_isIncluded:: other =", tmp % toString()
  !  gridMask_isIncluded = (disabledMask == other)
  !end function gridMask_isIncluded


  function gridMask_getShape(self) result (shape)
    !< grid shape
    class(shr_gridMask), intent(in) :: self
    integer :: shape(2)
    shape(1) = size(self % mask, dim=1)
    shape(2) = size(self % mask, dim=2)
  end function gridMask_getShape

end module shr_gridMask_mod

