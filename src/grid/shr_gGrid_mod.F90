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

  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_gGridArrayMap_mod, only: shr_gGridArrayMap, shr_gridArrayMapBuilder, shr_igGridArrayMap
  use shr_gridShape_mod, only: shr_gridShape
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gridcell_mod, only: shr_gridcell

  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_coord_mod, only: shr_coord
  use shr_strings_mod, only: string

  implicit none

  private

  public :: shr_gGrid

  integer, parameter :: SHR_GC_BOUNDS_TOP_LEFT = 1
  integer, parameter :: SHR_GC_BOUNDS_BOTTOM_RIGHT = 2

  !< interface
  type, abstract :: shr_igGrid
  end type shr_igGrid

  !< implementation
  type, extends(shr_igGrid) :: shr_gGrid
    class(shr_igGridDescriptor), allocatable :: gridDescriptor
    class(shr_igGridArrayMap), allocatable :: gridmap
  contains
    procedure :: init
    !< getters
    procedure :: getGridDescriptor
    procedure :: getGridMap

    procedure :: getShape !< gridShape

    !procedure :: getIndices !< by coordinates, returns shr_gridcellIndex
    procedure :: getIndicesByGrid
    generic :: getIndices => getIndicesByGrid

    procedure :: getBoundaryGridcell
    procedure :: fitsIn_byGridDescriptor
    procedure :: fitsIn_byGrid
    generic :: fitsIn => fitsIn_byGrid, fitsIn_byGridDescriptor

    procedure :: combine
    generic :: operator(+) => combine

    procedure :: equal
    generic :: operator(==) => equal

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
    class(shr_gGrid), intent(in) :: grid
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
    class(shr_gGrid), intent(in) :: grid

    real(kind=sp) :: halfres
    type(shr_gridBounds) :: bounds
    type(shr_coord) :: cTopLeft, cBottomRight
    type(shr_gridcellIndex), allocatable :: gIndicesTL(:), gIndicesBR(:)
    integer :: startlat, endlat
    integer :: startlon, endlon
    class(shr_iGGridDescriptor), allocatable :: currentSelfGDescriptor
    type(string) :: tmp
    type(shr_gridcell) :: topLeftGc, bottomRightGc

    currentSelfGDescriptor = self % getGridDescriptor()
    !tmp = currentSelfGDescriptor % toString()
    !write(*,*) "gridMask_mod:: findIndices:: current descriptor =", tmp % toString()
    !tmp = gDescriptor % toString()
    !write(*,*) "gridMask_mod:: findIndices:: (argument) gDescriptor =", tmp % toString()

    !if (.not. currentSelfGDescriptor % fitsIn(gDescriptor)) then
    if (.not. self % fitsIn(grid)) then
      call raiseError(__FILE__, "findIndices", &
          "Requested 'grid' bounds do not fit in current gridMask")
    end if

    topLeftGc = grid % getBoundaryGridcell(SHR_GC_BOUNDS_TOP_LEFT)
    bottomRightGc = grid % getBoundaryGridcell(SHR_GC_BOUNDS_BOTTOM_RIGHT)

    !< todo: refactor (somehow) top-left and bottom-right coordinates
    !< from bounds get top-left and bottom-right coordinates
    !< center of coordinate (it enforeces to select a unique gIndices)
    !halfRes = gDescriptor % getResolution() / 2.
    !bounds = gDescriptor % getBounds()
    ! todo: change east vs west, wrong
    cTopLeft = topLeftGc % getCenter() !shr_coord( bounds % getNorth() - halfRes, bounds % getEast() - halfres)
    cBottomRight = bottomRightGc % getCenter() !shr_coord( bounds % getSouth() + halfres, bounds % getWest() + halfres)
    !write(*,*) "gridMask_mod:: findIndices:: topLeft = ", &
    !    bounds % getNorth() - halfRes, &
    !    bounds % getEast() - halfres
    !write(*,*) "gridMask_mod:: findIndices:: bottomright= ", &
    !    bounds % getSouth() + halfres, &
    !    bounds % getWest() + halfres

    !< discover array indices
    !< find array indices from top-left
    !gIndicesTL = idxMapping % getIndex(cTopLeft) !< only 1
    gIndicesTL = self % gridmap % getIndex(cTopLeft)
    !write(*,*) "gridMask_mod:: findIndices:: top left indices found = ", size(gIndicesTL)
    !< find array indices from bottom-right
    !gIndicesBR = idxMapping % getIndex(cBottomRight) !< only 1
    gIndicesBR = self % gridmap % getIndex(cBottomRight)
    !write(*,*) "gridMask_mod:: findIndices:: bottom right indices found = ", size(gIndicesBR)

    !< todo: initialize shr_gridBoundIncides with 2 coordinates directly
    !< gBoundIndices(coord1, coord2)

    startlat = gIndicesTL(1) % idxLat
    endlat = gIndicesBR(1) % idxLat
    startlon = gIndicesTL(1) % idxLon
    endlon =  gIndicesBR(1) % idxLon
    call getIndicesByGrid % init(startlat, endlat, startlon, endlon)
  end function getIndicesByGrid


  logical function equal(self, other)
    !< true if 'self' and 'other' have the same attributes
    class(shr_gGrid), intent(in) :: self
    class(shr_gGrid), intent(in) :: other
    logical :: hasSameGDescriptor, hasSameGridmap

    hasSameGDescriptor = (self % gridDescriptor == other % getGridDescriptor())
    !write(*,*) "shr_gGrid:: equal:: hasSameGDescriptor =", hasSameGDescriptor
    hasSameGridmap = (self % gridmap == other % getGridmap())
    !write(*,*) "shr_gGrid:: equal:: hasSameGridMap =", hasSameGridmap
    equal = (hasSameGDescriptor .and. hasSameGridmap)
  end function equal


  type(shr_gGrid) function combine(self, other)
    !< combine 'self' and 'other'
    class(shr_gGrid), intent(in) :: self
    class(shr_gGrid), intent(in) :: other
    class(shr_iGGridDescriptor), allocatable :: newGdesc
    type(shr_gGridArrayMap) :: newGridMap
    newGdesc = self % getGridDescriptor() + other % getGridDescriptor()
    newGridMap = shr_gridArrayMapBuilder(newGdesc)
    call combine % init(newGdesc, newGridMap)
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
    type(shr_coord) :: boundCoord
    type(shr_gridcell), allocatable :: boundaryGcs(:)

    bounds = self % gridDescriptor % getBounds()

    if (position == SHR_GC_BOUNDS_TOP_LEFT) then
      boundCoord = bounds % getCoordinateNorthWest()
    else if (position == SHR_GC_BOUNDS_BOTTOM_RIGHT) then
      boundCoord = bounds % getCoordinateSouthEast()
    else
      call raiseError(__FILE__, "getBoundaryGridcell", &
          "Unexpected position requested")
    end if

    !< todo : create new class shr_gGridCellsMap(gDescriptor, gAxis::lat, gAxis::lon)
    !<    shr_gGridArrayMap should be renamed to shr_gGridArrayMap(gDescriptor, latmapping, lonmapping)

    !boundaryGcs = self % gridcellsMap % get(boundCoord)
    if (size(boundaryGcs) > 1) then
      call raiseError(__FILE__, "getBoundaryGridcell", &
          "Found more gridcells(?) than expected(1)")
    end if
    newgc = boundaryGcs(1)
  end function getBoundaryGridcell

end module shr_gGrid_mod
