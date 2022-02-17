!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGrid_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> grid interface
!>
!------------------------------------------------------------------------------
module shr_gGrid_mod

  use shr_precision_mod, only: sp
  use shr_error_mod, only: raiseError

  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor, shr_gGridDescriptor
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap, shr_gridArrayMapBuilder, shr_igGridArrayMap
  use shr_gridShape_mod, only: shr_gridShape
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gridcell_mod, only: shr_gridcell
  use shr_gGridCellsMap_mod, only: shr_gGridCellsMap, shr_gGridCellsMapBuilder

  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_coord_mod, only: shr_coord
  use shr_strings_mod, only: string

  implicit none

  private

  public :: shr_gGrid, shr_igGrid, shr_igGridBuilder

  integer, parameter :: SHR_GC_BOUNDS_TOP_LEFT = 1
  integer, parameter :: SHR_GC_BOUNDS_BOTTOM_RIGHT = 2

  !< interface
  type, abstract :: shr_igGrid
  contains
    procedure(iface_init), deferred :: init

    procedure(iface_getGridDescriptor), deferred :: getGridDescriptor
    procedure(iface_getshape), deferred :: getShape
    procedure(iface_toString), deferred :: toString
    procedure(iface_getResolution), deferred :: getResolution

    procedure(iface_getIndicesByGrid), deferred :: getIndicesByGrid
    generic :: getIndices => getIndicesByGrid

    procedure(iface_getBoundaryGridcell), deferred :: getBoundaryGridcell

    procedure(iface_fitsIn_byGridDescriptor), deferred :: fitsIn_byGridDescriptor
    procedure(iface_fitsIn_byGrid), deferred :: fitsIn_byGrid
    generic :: fitsIn => fitsIn_byGrid, fitsIn_byGridDescriptor

    procedure(iface_combine), deferred :: combine
    generic :: operator(+) => combine

    procedure(iface_equal), deferred :: equal
    generic :: operator(==) => equal
  end type shr_igGrid


  abstract interface
    subroutine iface_init(self, gridDescriptor, gridMap)
      import :: shr_igGrid, shr_igGridDescriptor, shr_igGridArrayMap
      !< initialize
      class(shr_igGrid), intent(inout) :: self
      class(shr_igGridDescriptor), intent(in) :: gridDescriptor
      class(shr_igGridArrayMap), intent(in) :: gridMap
    end subroutine iface_init

    type(shr_gridShape) function iface_getShape(self)
      import :: shr_igGrid, shr_gridShape
      !< return grid shape
      class(shr_igGrid), intent(in) :: self
    end function iface_getShape

    type(string) function iface_toString(self)
      import :: string, shr_igGrid
      !< string representation of shr_igGrid
      class(shr_igGrid), intent(in) :: self
    end function iface_toString

    logical function iface_fitsIn_byGrid(self, grid)
      import :: shr_igGrid, shr_iGGridDescriptor
      !< true if 'grid' gDescriptor'' fits in self % gGridDescriptor
      class(shr_igGrid), intent(in) :: self
      class(shr_igGrid), intent(in) :: grid
      class(shr_iGGridDescriptor), allocatable :: gDescriptor
    end function iface_fitsIn_byGrid

    logical function iface_fitsIn_byGridDescriptor(self, gDescriptor)
      import :: shr_igGrid, shr_iGGridDescriptor
      !< true if gDescriptor fits in self % gGridDescriptor
      class(shr_igGrid), intent(in) :: self
      class(shr_iGGridDescriptor), intent(in) :: gDescriptor
    end function iface_fitsIn_byGridDescriptor

    function iface_getGridDescriptor(self) result (gDescriptor)
      import :: shr_igGrid, shr_igGridDescriptor
      !< returns grid descriptor
      class(shr_igGrid), intent(in) :: self
      class(shr_iGGridDescriptor), allocatable :: gDescriptor !< output
    end function iface_getGridDescriptor

    type(shr_gridBoundIndices) function iface_getIndicesByGrid(self, grid)
      import :: shr_igGrid, shr_gridBoundIndices
      !< It calculates indices from 'grid' boundaries
      !< returns shr_gridBoundIndices from given 'gDescriptor'
      class(shr_igGrid), intent(in) :: self
      class(shr_igGrid), intent(in) :: grid
    end function iface_getIndicesByGrid

    type(shr_gridcell) function iface_getBoundaryGridcell(self, position) result (newgc)
      import :: shr_igGrid, shr_gridcell
      !< Returns a gridcell from on of the corners of its boundaries
      !< Position is an integer with requests mapped as:
      !< -north east
      !< -south east
      !< -north west
      !< -south west
      class(shr_igGrid), intent(in) :: self
      integer, intent(in) :: position
    end function iface_getBoundaryGridcell

    logical function iface_equal(self, other)
      import :: shr_igGrid
      !< true if 'self' and 'other' have the same attributes
      class(shr_igGrid), intent(in) :: self
      class(shr_igGrid), intent(in) :: other
    end function iface_equal

    function iface_combine(self, other) result (newGrid)
      import :: shr_igGrid
      !< combine 'self' and 'other'
      class(shr_igGrid), intent(in) :: self
      class(shr_igGrid), intent(in) :: other
      class(shr_igGrid), allocatable :: newGrid
    end function iface_combine

    function iface_getResolution(self) result (res)
      import :: shr_igGrid, sp
      !< return grid resolution
      class(shr_igGrid), intent(in) :: self
      real(kind=sp) :: res !< output
    end function iface_getResolution
  end interface


  !< implementation
  type, extends(shr_igGrid) :: shr_gGrid
    class(shr_igGridDescriptor), allocatable :: gridDescriptor
    class(shr_igGridArrayMap), allocatable :: gridmap
    class(shr_gGridCellsMap), allocatable :: gridcellsMap
  contains
    procedure :: init
    !< getters
    procedure :: getGridDescriptor
    procedure :: getGridMap

    procedure :: getResolution => gGrid_getResolution
    procedure :: getShape !< gridShape

    !procedure :: getIndices !< by coordinates, returns shr_gridcellIndex
    procedure :: getIndicesByGrid
    !generic :: getIndices => getIndicesByGrid

    procedure :: getBoundaryGridcell
    procedure :: fitsIn_byGridDescriptor
    procedure :: fitsIn_byGrid
    !generic :: fitsIn => fitsIn_byGrid, fitsIn_byGridDescriptor

    procedure :: combine
    !generic :: operator(+) => combine

    procedure :: equal
    !generic :: operator(==) => equal

    procedure :: toString
  end type shr_gGrid

contains


  subroutine init(self, gridDescriptor, gridMap)
    !< initialize
    class(shr_gGrid), intent(inout) :: self
    class(shr_igGridDescriptor), intent(in) :: gridDescriptor
    class(shr_igGridArrayMap), intent(in) :: gridMap
    allocate(self % gridDescriptor, source = gridDescriptor)
    allocate(self % gridMap, source = gridMap)

    !< todo: temporary allocate gridcellsMap to pass unit tests
    !<      Provide as an argument later on
    allocate(self % gridcellsMap)
    self % gridcellsMap = shr_gGridCellsMapBuilder(gridDescriptor)
  end subroutine init


  function getGridMap(self) result (gridMap)
    !< returns gridMap
    class(shr_gGrid), intent(in) :: self
    class(shr_igGridArrayMap), allocatable :: gridMap !< output
    allocate(gridMap, source = self % gridmap)
  end function getGridMap


  function getGridDescriptor(self) result (gDescriptor)
    !< returns grid descriptor
    class(shr_gGrid), intent(in) :: self
    class(shr_iGGridDescriptor), allocatable :: gDescriptor !< output
    allocate(gDescriptor, source = self % gridDescriptor)
  end function getGridDescriptor


  type(shr_gridShape) function getShape(self)
    !< return grid shape
    class(shr_gGrid), intent(in) :: self
    getShape = self % gridmap % getShape()
  end function getShape


  logical function fitsIn_byGrid(self, grid)
    !< true if 'grid' gDescriptor'' fits in self % gGridDescriptor
    class(shr_gGrid), intent(in) :: self
    class(shr_igGrid), intent(in) :: grid
    class(shr_iGGridDescriptor), allocatable :: gDescriptor
    gDescriptor = grid % getGridDescriptor()
    fitsIn_byGrid = self % gridDescriptor % fitsIn(gDescriptor)
  end function fitsIn_byGrid


  logical function fitsIn_byGridDescriptor(self, gDescriptor)
    !< true if gDescriptor fits in self % gGridDescriptor
    class(shr_gGrid), intent(in) :: self
    class(shr_iGGridDescriptor), intent(in) :: gDescriptor
    fitsIn_byGridDescriptor = self % gridDescriptor % fitsIn(gDescriptor)
  end function fitsIn_byGridDescriptor


  type(shr_gridBoundIndices) function getIndicesByGrid(self, grid)
    !< It calculates indices from 'grid' boundaries
    !< returns shr_gridBoundIndices from given 'gDescriptor'
    class(shr_gGrid), intent(in) :: self
    class(shr_igGrid), intent(in) :: grid

    type(shr_coord) :: cTopLeft, cBottomRight
    type(shr_gridcellIndex), allocatable :: gIndicesTL(:), gIndicesBR(:)
    integer :: tmpidx
    !integer :: startlat, endlat
    !integer :: startlon, endlon
    !type(string) :: tmp
    type(shr_gridcell) :: topLeftGc, bottomRightGc

    if (.not. self % fitsIn(grid)) then
      call raiseError(__FILE__, "findIndices", &
          "Requested 'grid' bounds do not fit in current gridMask")
    end if

    topLeftGc = grid % getBoundaryGridcell(SHR_GC_BOUNDS_TOP_LEFT) !< north - west
    bottomRightGc = grid % getBoundaryGridcell(SHR_GC_BOUNDS_BOTTOM_RIGHT) !< south - east

    cTopLeft = topLeftGc % getCenter()
    cBottomRight = bottomRightGc % getCenter()

    !< discover array indices
    gIndicesTL = self % gridmap % getIndex(cTopLeft)
    !write(*,*) "gridMask_mod:: findIndices:: top left indices found = ", size(gIndicesTL)
    gIndicesBR = self % gridmap % getIndex(cBottomRight)
    !write(*,*) "gridMask_mod:: findIndices:: bottom right indices found = ", size(gIndicesBR)

    !! todo: change east vs west, wrong
    !< currently swap values
    tmpidx = gIndicesBR(1) % idxlon
    gIndicesBR(1) % idxlon = gIndicesTL(1) % idxlon
    gIndicesTL(1) % idxlon = tmpidx

    call getIndicesByGrid % init(gIndicesTL(1), gIndicesBR(1))
  end function getIndicesByGrid


  logical function equal(self, other)
    !< true if 'self' and 'other' have the same attributes
    class(shr_gGrid), intent(in) :: self
    class(shr_igGrid), intent(in) :: other
    logical :: hasSameGDescriptor, hasSameGridmap
    type(shr_gGrid) :: otherGrid

    if (.not. same_type_as(self, other)) then
      equal = .false.
      return
    end if

    select type(o => other)
    type is (shr_gGrid)
      otherGrid = o
    class default
      !< unexpected error
    end select

    hasSameGDescriptor = (self % gridDescriptor == otherGrid % getGridDescriptor())
    !write(*,*) "shr_gGrid:: equal:: hasSameGDescriptor =", hasSameGDescriptor
    hasSameGridmap = (self % gridmap == otherGrid % getGridmap())
    !write(*,*) "shr_gGrid:: equal:: hasSameGridMap =", hasSameGridmap
    equal = (hasSameGDescriptor .and. hasSameGridmap)
  end function equal


  function combine(self, other) result (newGrid)
    !< combine 'self' and 'other'
    class(shr_gGrid), intent(in) :: self
    class(shr_igGrid), intent(in) :: other
    class(shr_igGrid), allocatable :: newGrid

    class(shr_iGGridDescriptor), allocatable :: newGdesc
    type(shr_gGridArrayMap) :: newGridMap
    newGdesc = self % getGridDescriptor() + other % getGridDescriptor()
    newGridMap = shr_gridArrayMapBuilder(newGdesc)
    allocate(shr_gGrid :: newGrid)
    call newGrid % init(newGdesc, newGridMap)
  end function combine


  type(string) function toString(self)
    !< string representation of shr_gGrid
    class(shr_gGrid), intent(in) :: self
    toString = self % gridmap % toString()
  end function toString


  type(shr_gridcell) function getBoundaryGridcell(self, position) result (newgc)
    !< Returns a gridcell from on of the corners of its boundaries
    !< Position is an integer with requests mapped as:
    !< -north east
    !< -south east
    !< -north west
    !< -south west
    class(shr_gGrid), intent(in) :: self
    integer, intent(in) :: position
    type(shr_gridBounds) :: bounds
    type(shr_coord) :: boundCoord, center
    type(shr_gridcell), allocatable :: boundaryGcs(:)
    type(shr_gGridCellsMap) :: gcsmap

    bounds = self % gridDescriptor % getBounds()

    if (position == SHR_GC_BOUNDS_TOP_LEFT) then
      boundCoord = bounds % getCoordinateNorthWest()
    else if (position == SHR_GC_BOUNDS_BOTTOM_RIGHT) then
      boundCoord = bounds % getCoordinateSouthEast()
    else
      call raiseError(__FILE__, "getBoundaryGridcell", &
          "Unexpected position requested")
    end if

    boundaryGcs = self % gridcellsMap % getGridCells(boundCoord)

    !boundaryGcs = self % gridcellsMap % get(boundCoord)
    if (size(boundaryGcs) /= 1) then
      write(*,*) "shr_gGrid_mod:: getBoundaryGridcell:: boundaryGcs =", size(boundaryGcs)
      call raiseError(__FILE__, "getBoundaryGridcell", &
          "Found more gridcells(?) than expected(1)")
    end if
    newgc = boundaryGcs(1)
  end function getBoundaryGridcell


  function gGrid_getResolution(self) result (res)
    !< return grid resolution
    class(shr_gGrid), intent(in) :: self
    real(kind=sp) :: res !< output
    res = self % gridDescriptor % getResolution()
  end function gGrid_getResolution


  function shr_igGridBuilder(resolution, bounds) result (newGGrid)
    !< build a new igGrid
    real(kind=sp), intent(in) :: resolution
    type(shr_gridBounds), intent(in) :: bounds
    class(shr_igGrid), allocatable :: newGGrid !< output

    class(shr_iGGridDescriptor), allocatable :: newGdescriptor
    class(shr_igGridArrayMap), allocatable :: newGridArrayMap

    allocate(shr_gGridDescriptor :: newGdescriptor)
    call newGdescriptor % init(resolution, bounds)

    newGridArrayMap = shr_gridArrayMapBuilder(newGdescriptor)

    allocate(shr_gGrid :: newGGrid)
    call newGGrid % init(newGdescriptor, newGridArrayMap)
  end function shr_igGridBuilder

end module shr_gGrid_mod
