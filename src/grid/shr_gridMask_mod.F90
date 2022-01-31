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
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gGrid_mod, only: shr_gGrid

  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gridShape_mod, only: shr_gridShape
  use shr_coord_mod, only: shr_coord

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

    procedure(iface_get), deferred :: get
    procedure(iface_getGrid), deferred :: getGrid

    procedure(iface_equal_scalar_logical), deferred :: equal_scalar_logical
    procedure(iface_equal_rawMask), deferred :: equal_rawMask
    procedure(iface_equal_gridMask), deferred :: equal_gridMask
    generic :: operator(==) => equal_scalar_logical, equal_gridMask, &
        equal_rawMask

    procedure(iface_op_gridMask), deferred :: and_gridMask
    generic :: operator(.and.) => and_gridMask
    procedure(iface_op_gridMask), deferred :: or_gridMask
    generic :: operator(.or.) => or_gridMask
    procedure(iface_any), deferred :: any

    procedure(iface_expand), deferred :: expand
    procedure(iface_select), deferred :: select

    procedure(iface_set_by_lmask), deferred :: set_by_lmask
    procedure(iface_set_by_gridmask), deferred :: set_by_gridMask
    generic :: set => set_by_gridMask, set_by_lmask

    procedure(iface_toString), deferred :: toString
    procedure(iface_getShape), deferred :: getShape
  end type


  abstract interface
    subroutine iface_gridMask_initialize_by_larray(self, grid, lmask)
      import :: shr_igridMask, shr_gGrid
      !< gridMask initialization
      class(shr_igridMask), intent(inout) :: self
      class(shr_gGrid), intent(in) :: grid
      logical, intent(in) :: lmask(:,:)
    end subroutine iface_gridMask_initialize_by_larray

    subroutine iface_gridMask_initialize(self, grid, default)
      import :: shr_igridMask, shr_gGrid
      !< gridMask initialization
      class(shr_igridMask), intent(inout) :: self
      class(shr_gGrid), intent(in) :: grid
      logical, intent(in), optional :: default !< define default value (def: true)
    end subroutine iface_gridMask_initialize

    function iface_get(self, gBoundIndices) result (newMask)
      import :: shr_IgridMask, shr_gridBoundIndices
      !< returns current mask
      !< in case gBoundIndices are defined it returns the selected indices
      !< those indices must be valid
      class(shr_IgridMask), intent(in) :: self
      type(shr_gridBoundIndices), intent(in), optional :: gBoundIndices
      logical, allocatable :: newMask(:,:) !< output
    end function iface_get

    function iface_getGrid(self) result(newGDescriptor)
      import :: shr_IgridMask, shr_gGrid
      !< returns self gridDescriptor
      class(shr_IgridMask), intent(in) :: self
      class(shr_gGrid), allocatable :: newGDescriptor !< output
    end function iface_getGrid

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

    function iface_expand(self, grid, default) result (newGMask)
      import :: shr_igridMask, shr_gGrid
      !< returns a new shr_gridMask with an expanded grid
      !< - 'self' must fit into 'gDescriptor'
      !< - mask remains the same
      !< default argument define which values to set for new mask cells
      class(shr_igridMask), intent(in) :: self
      class(shr_gGrid), intent(in) :: grid
      logical, intent(in), optional :: default
      class(shr_igridMask), allocatable :: newGMask !< output
    end function iface_expand

    function iface_select(self, grid) result (newGMask)
      import :: shr_igridMask, shr_gGrid
      !< select a new shr_gridMask according to grid
      !< new grid must fit self % grid
      class(shr_igridMask), intent(in) :: self
      class(shr_gGrid), intent(in) :: grid
      class(shr_igridMask), allocatable :: newGMask !< output
    end function iface_select

    function iface_op_gridMask(self, other) result (newMask)
      import :: shr_igridMask
      !< returns a new gridMask with matching gridcells
      !< both must have the same size
      class(shr_igridMask), intent(in) :: self
      class(shr_igridMask), intent(in) :: other
      class(shr_igridMask), allocatable :: newMask !< output
    end function iface_op_gridMask

    subroutine iface_set_by_lmask(self, mask, gBindices)
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
    end subroutine iface_set_by_lmask

    subroutine iface_set_by_gridmask(self, gridMask)
      import :: shr_igridMask
      !< set values of 'gridMask' into 'self'
      !< 'gridMask' shape must be consistent with 'self'
      !< - gridMask must fit in 'self'
      class(shr_igridMask), intent(inout) :: self
      class(shr_igridMask), intent(in) :: gridMask
    end subroutine iface_set_by_gridmask

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
    class(shr_gGrid), allocatable :: grid

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

    procedure :: get
    procedure :: getGrid => gridMask_getGrid

    procedure, pass(self) :: copy_rev_array
    procedure :: copy_gridMask
    generic :: assignment(=) => copy_gridMask, copy_rev_array

    procedure :: equal_scalar_logical => gridMask_equal_scalar_logical
    procedure :: equal_rawMask => gridMask_equal_rawMask
    procedure :: equal_gridMask => gridMask_equal_gridMask

    !< same grid descriptor
    procedure :: or_gridMask => gridMask_or_gridMask
    procedure :: and_gridMask => gridMask_and_gridMask

    procedure :: reverse_gridMask_func
    procedure :: reverse => reverse_gridMask
    generic :: operator(.not.) => reverse_gridMask_func

    procedure, private :: isValidMask => isValidBy2dArray

    procedure :: any => any_gridMask

    !< generic => set
    procedure :: set_by_gridmask => gridMask_set_by_gridmask
    procedure :: set_by_lmask => gridMask_set_by_lmask

    procedure :: select => gridMask_select
    procedure :: expand => gridMask_expand
    procedure :: toString => gridMask_toString
    procedure :: getShape => gridMask_getShape
  end type shr_gridMask

contains

  subroutine gridMask_initialize_by_larray(self, grid, lmask)
    !< gridMask initialization
    !< lmask shape must be consistent with gridDescriptor
    class(shr_gridMask), intent(inout) :: self
    class(shr_gGrid), intent(in) :: grid
    logical, intent(in) :: lmask(:,:)

    integer :: lshape(2), inMaskShape(2)
    type(stringCollection) :: tmpCollection, tmpCollection1
    type(string), allocatable :: tmp(:), tmp1(:)

    call self % initialize(grid)

    !< lmask shape matches grid shape?
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


  subroutine gridMask_initialize(self, grid, default)
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    class(shr_gGrid), intent(in) :: grid
    logical, intent(in), optional :: default !< define default value (def: true)

    logical :: inDefault
    integer :: nlats, nlons
    type(shr_gridShape) :: gShape

    inDefault = .true.
    if (present(default)) inDefault = default
    allocate(self % grid, source = grid)

    gShape = self % grid % getShape()
    nlats = gShape % getLats()
    nlons = gShape % getLons()
    !write(*,*) "gridMask_mod:: gridMask_initialize:: nlats (dim=1) = ", nlats
    !write(*,*) "gridMask_mod:: gridMask_initialize:: nlons (dim=2) = ", nlons
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


  function get(self, gBoundIndices) result (newMask)
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
  end function get


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
    logical :: hasSameGrid, hasSameMask
    type(shr_gridMask) :: ogMask

    select type (o => other)
    type is (shr_gridMask)
      ogMask = o
    class default
      gridMask_equal_gridMask = .false.
    end select

    hasSameGrid = (self % grid == ogMask % getGrid())
    hasSameMask = all(self % mask .eqv. ogMask % mask)
    gridMask_equal_gridMask = (hasSameGrid .and. hasSameMask)
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
    lmask = (self % mask .and. other % get() )

    allocate(shr_gridMask :: newMask)
    call newMask % init(self % grid, lmask)
  end function gridMask_and_gridMask


  function gridMask_or_gridMask(self, other) result (newMask)
    !< returns a new gridMask with matching gridcells
    !< both must have the same size
    !<
    !< gridMask(T T F) = gridMask(T T F) .and. gridMask(F T F)
    class(shr_gridMask), intent(in) :: self
    class(shr_igridMask), intent(in) :: other
    class(shr_igridMask), allocatable :: newMask !< output
    logical, allocatable :: lmask(:,:)
    !< copy shape
    allocate(lmask, mold = self % mask)
    lmask = (self % mask .or. other % get() )

    allocate(shr_gridMask :: newMask)
    call newMask % init(self % grid, lmask)
  end function gridMask_or_gridMask


  subroutine reverse_gridMask(self)
    !< modify mask bitwise complement (.not.)
    class(shr_gridMask), intent(inout) :: self
    self % mask = .not. (self % mask)
  end subroutine reverse_gridMask


  type(shr_gridMask) function reverse_gridMask_func(self) result (newGM)
    !< bitwise complement (.not.)
    class(shr_gridMask), intent(in) :: self
    call newGM % init(self % getGrid())
    newGM % mask = .not. (self % mask)
  end function reverse_gridMask_func


  subroutine copy_gridMask(self, other)
    !< copy all attributes from 'other' to 'self'
    class(shr_gridMask), intent(inout) :: self
    class(shr_gridMask), intent(in) :: other
    if (allocated(self % mask)) deallocate(self % mask)
    if (allocated(self % grid)) deallocate(self % grid)
    allocate(self % mask, source = other % mask)
    allocate(self % grid, source = other % grid)
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

    type(shr_gAxis) :: tmpGAxis
    integer :: nlats, nlons
    type(shr_gridShape) :: gshape

    gshape = self % grid % getShape()
    nlats = gshape % getLats()
    nlons = gshape % getLons()

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


  function gridMask_getGrid(self) result (newGrid)
    !< returns self grid
    class(shr_gridMask), intent(in) :: self
    class(shr_gGrid), allocatable :: newGrid !< output
    allocate(newGrid, source = self % grid)
  end function gridMask_getGrid


  type(string) function gridMask_toString(self)
    !< mask to string type
    !<
    !< bounds % toString()
    !< 'T T'
    !< 'T T'
    !< 'F T'
    !<
    class(shr_gridMask), intent(in) :: self
    integer :: nlats, ilat
    type(shr_gAxis) :: latAxis
    character(:), allocatable :: tmp
    type(string) :: strGrid
    character(500) :: t
    type(shr_gridShape) :: gshape
    strGrid =  self % grid % toString()

    gshape = self % grid % getShape()
    nlats =  gshape % getLats()
    tmp = "" ! initialize
    do ilat = 1, nlats-1
      write(t, *) self % mask(ilat, :)
      tmp = tmp // "'" // trim(adjustl(t)) // "'" // NEW_LINE('a')
    end do
    write(t, *) self % mask(nlats, :)
    tmp = tmp // "'" // trim(adjustl(t)) // "'"
    gridMask_toString = strGrid + new_line('A') + string(tmp)
  end function gridMask_toString


  subroutine shr_gridMask_cast(obj, gMask)
    !< Cast from * to shr_gridMask
    class(*), intent(in) :: obj
    class(shr_igridMask), allocatable, intent(out) :: gMask

    select type(o => obj)
    class is(shr_igridMask)
      gMask = o
    class default
      call raiseError(__FILE__, &
          "shr_gridMask_cast", &
          "Unexpected type found instead of 'shr_gridMask'")
    end select

  end subroutine shr_gridMask_cast


  function gridMask_select(self, grid) result (newGMask)
    !< select a new shr_gridMask according to gDescriptor
    !< new 'gDescriptor' must fit self % gridDescriptor
    !< the output gridMask will have 'gDescriptor' as grid descriptor
    !< and the mask values matching 'self'
    class(shr_gridMask), intent(in) :: self
    class(shr_gGrid), intent(in) :: grid
    class(shr_igridMask), allocatable :: newGMask !< output

    logical, allocatable :: newLmask(:,:)
    !type(shr_gridIndicesMapping) :: idxMapping
    type(shr_gridBoundIndices) :: gBoundIndices
    type(string) :: tmp
    logical, allocatable :: tmpMask(:,:)
    class(shr_iGGridDescriptor), allocatable :: gDescriptor


    if (.not. self % grid % fitsIn(grid) ) then
      call raiseError(__FILE__, "select", &
          "'grid' argument does not fit in the current shr_gridDomain")
    end if

    !< discover array indices
    !call idxMapping % init(self % getGridDescriptor())
    gDescriptor = grid % getGridDescriptor()
    tmp = gDescriptor % toString()
    write(*,*) "shr_gridMask_mod:: gridMask_select:: grid % getGridDescriptor()= ", tmp % toString()
    gBoundIndices = self % grid % getIndices(grid)
    !gBoundIndices = self % findIndices(gDescriptor, idxMapping)
    write(*,*) "gridMAsk_mod:: gridMask_select:: gBoundIndices (n, s, e, w)=", &
        gBoundIndices % startRow, gBoundIndices % endRow, &
        gBoundIndices % startCol, gBoundIndices % endCol

    allocate(shr_gridMask :: newGMask)
    call newGMask % init(grid, default = .false.)!, newLmask)
    !tmp = gDescriptor % toString()
    !write(*,*) "gridMAsk_mod:: gridMask_select:: newGMask gDescriptor =", tmp % toString()
    tmpMask = self % get(gBoundIndices)
    call newGMask % set(tmpMask)
  end function gridMask_select


  function gridMask_expand(self, grid, default) result (newGMask)
    !< returns a new shr_gridMask with an expanded grid
    !< - 'self' must fit into 'gDescriptor'
    !< - mask remains the same
    !< default optional argument define which values get for new mask cells
    class(shr_gridMask), intent(in) :: self
    !type(shr_gridBounds), intent(in) :: bounds
    class(shr_gGrid), intent(in) :: grid
    logical, intent(in), optional :: default
    class(shr_igridMask), allocatable :: newGMask

    type(shr_gridBoundIndices) :: gBoundIndices
    logical, allocatable :: newLMask(:,:)

    !< new bounds fit in current gridMask?
    if (.not. grid % fitsIn(self % getGrid()) ) then
      call raiseError(__FILE__, "expand", &
          "'grid' argument does not fit in the current shr_gridDomain")
    end if

    !< temporary mask to initialze mask with proper dimensions
    allocate(shr_gridMask :: newGMask)
    call newGMask % init(grid, default)

    !call newGMask % set(self % mask, gBoundIndices)
    call newGMask % set(self) !< todo: set grid mask from 'grid' into newGMask
  end function gridMask_expand


  subroutine gridMask_set_By_Gridmask(self, gridMask)
    !< It directly sets 'gridMask' mask values into 'self'.
    !< gridMask must fit into self otherwise an error is raised
    class(shr_gridMask), intent(inout) :: self
    class(shr_IgridMask), intent(in) :: gridMask

    clasS(shr_iGGridDescriptor), allocatable :: argGDescriptor
    type(shr_gridBoundIndices) :: gBoundIndices
    logical, allocatable :: lmask(:,:)
    class(shr_gGrid), allocatable :: argGrid

    argGrid = gridMask % getGrid()
    gBoundIndices = self % grid % getIndices(argGrid)
    lmask = gridMask % get()
    call self % set(lmask, gBoundIndices)
  end subroutine gridMask_set_By_Gridmask


  subroutine gridMask_set_by_lmask(self, mask, gBindices)
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

    !write(*,*) "gridMask_mod:: set:: self % mask shape (rows, cols)? ", shape(self % mask)
    !write(*,*) "gridMask_mod:: set:: self % mask args, rows (start, end) =", startRow, endRow
    !write(*,*) "gridMask_mod:: set:: self % mask args, cols (start, end) =", startCol, endCol
    !write(*,*) "gridMask_mod:: set:: given mask shape? ", shape(mask)
    !write(*,*) "gridMask_mod:: set:: given mask args, nrows, ncols = 1:", nrows, ", 1:", ncols

    !write(*,*) "gridMask_mod:: set:: self % mask(",startRow,":",endRow,&
                !",",startCol,":",endCol,&
                !") = mask(1:",nrows,",1:",ncols,")"
    self % mask(startRow:endRow, startCol:endCol) = mask(1:nrows,1:ncols)
  end subroutine gridMask_set_by_lmask


  function gridMask_getShape(self) result (shape)
    !< grid shape
    class(shr_gridMask), intent(in) :: self
    integer :: shape(2)
    shape(1) = size(self % mask, dim=1)
    shape(2) = size(self % mask, dim=2)
  end function gridMask_getShape

end module shr_gridMask_mod

