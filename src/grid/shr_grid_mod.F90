!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : grid_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> In charge to deal with coordinates and position
!>
!> 'Grid' takes the responsability to mange squared grid.
!> It is composed of 'gridcells'. Those are defined at the 
!> center of the gridcell.
!> 'Grid' also handles how it is decomposed and it takes 
!> care of a part of it. It is usefull for parallel
!> computation.
!> A grid can have enable/disabled gridcells. When disabled those
!> are completly ignored (e.g: sea).
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : common.fypp 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> 
!>
!------------------------------------------------------------------------------












! function_rspX
! 
! function_iX










!
! Prints the allocation array dimensions from an array which contains its dimensions 
!
! Args:
!   vname (str): array name which contains dimensions
!   nrank (int): how many rank generate
!
! Returns:
!   ( vname(1), vname(2), vname(3), ... ) 
!                


module shr_grid_mod

  use SHR_precision_mod, only: dp, sp 
  use SHR_error_mod, only: raiseError
  use SHR_arrayUtils_mod, only: unique, closestNumber2Index, PREFER_LAST, &
                        initArrayRange
  use shr_coord_mod, only: coord
  use shr_gridPartition_mod, only: grid_partition_type
  use shr_window_mod, only: window 
  use shr_window_mod, only: WINDOW_NCOORDS, WINDOW_NORTH, &
             WINDOW_SOUTH, WINDOW_EAST, WINDOW_WEST

  implicit none

  private

  public :: grid, coord
  public :: GRIDCELL_NNEIGHS, GRIDCELL_N_NEAST, GRIDCELL_N_NORTH, GRIDCELL_N_EAST, &
            GRIDCELL_N_SEAST, GRIDCELL_N_SOUTH, GRIDCELL_N_SWEST, GRIDCELL_N_WEST, &
            GRIDCELL_N_NWEST, gridcell


  integer, parameter :: GRIDCELL_NNEIGHS = 8 !< total number of neighbours
  integer, parameter :: GRIDCELL_N_NORTH = 1 !< north neighbour id
  integer, parameter :: GRIDCELL_N_NEAST = 2 !< north east neighbour id
  integer, parameter :: GRIDCELL_N_EAST = 3 !< east neighbour id
  integer, parameter :: GRIDCELL_N_SEAST = 4 !< south east neighbour id
  integer, parameter :: GRIDCELL_N_SOUTH = 5 !< south neighbour id
  integer, parameter :: GRIDCELL_N_SWEST = 6 !< south west neighbour id
  integer, parameter :: GRIDCELL_N_WEST = 7 !< west neighbour id
  integer, parameter :: GRIDCELL_N_NWEST = 8 !< north west neighbour id


!  type pGridcell !< pointer to gridcell type
!    type(gridcell), pointer :: p => null()
!  end type pGridcell


  type gridcell !< gridcell from the map
    integer :: idx !< gridcell index in repect to the overall grid (starting from top-left with 1 to ...)
    type(coord) :: center !< gridcell center
    real(kind=sp) :: resolution !< gridcell resolution
    type(window) :: limits !< limits of the gridcell n, s, e, w
    logical :: enabled !< data is used, otherwise it is ignored
  contains
    procedure :: contains
    procedure :: getNorth, getSouth
    procedure :: getEast, getWest
    procedure :: getSpatialIdxs 
    procedure :: toString => toString_gridcell

    procedure, private :: gridcell_eq
    generic :: operator(==) => gridcell_eq
  end type gridcell

  interface gridcell
    module procedure :: gridcell_constructor
  end interface


  type :: grid !< latitude and longitude have the same resolution
    real(kind=sp), allocatable :: lats(:) !< list all latitudes
    real(kind=sp), allocatable :: lons(:) !< list all longitudes
    real(kind=sp) :: resolution !< grid resolution
    type(window) :: limits !< max/min lats/lons of the grid

    type(gridcell), allocatable :: gridcells(:) !< gridcells
    type(grid_partition_type), allocatable :: partitions
  contains
    procedure :: constructor => oop_grid_constructor
    procedure :: hasNeighbourByIdx !< true if the given direction has a neighbour
    procedure :: hasNeighbourByGc !< true if the given direction has a neighbour
    generic :: hasNeighbour => hasNeighbourByIdx, hasNeighbourByGc
    procedure :: getNeighbourByGc !< it returns the pointer of the neighbouring gridcell. Error if it does not exists.
    procedure :: getNeighbourByIdx !< it returns the pointer of the neighbouring gridcell. Error if it does not exists.
    generic :: getNeighbour => getNeighbourByGc, getNeighbourByIdx

    procedure :: getSubGrid !< it returns a new grid with the given coordinates
    procedure :: fitsInBounds !< true if the given grid fits in the current one 
    procedure :: fitsInGrid !< true if the given grid fits in the current one 
    procedure :: fitsInCoord !< true if the given coordinate fits in the current one 
    generic :: fitsIn => fitsInBounds, fitsInCoord, fitsInGrid

    procedure :: isGlobal !< true if has the whole world
    procedure :: getLimits !< max/min for lat and lon
    procedure :: getResolution !< return resolution
    procedure :: getLatCoords !< get all latitude coordinates
    procedure :: getLonCoords !< get all longitude coordinates
    procedure :: latCoord2Index !< get all latitude indices from a lat coordinate
    procedure :: lonCoord2Index !< get all longitude indices from a long coordinate

    procedure :: getLatSize !< get the total number of latitudes
    procedure :: getLonSize !< get the total number of longitudes

    procedure :: getLandIndices !< global or local, it returns all land indices from requested domain
    procedure :: getLandBounds !< global or local, it returns gridcells indices bounds 

    ! gridcells related

    procedure :: countAllGridcells !< total number of gridcells
    procedure :: getGridcellBySpatialIndices !< Given lat and lon spatial array indices it returns a coordinate from
                                 !< the center of the gridcell
    procedure :: getGridcellByCoord !< given a coordinate it finds the gridcell
    generic :: getGridcell => getGridcellByCoord, getGridcellBySpatialIndices
    procedure :: isCoordEnabled !< true if the given coordinate is enabled

    procedure :: getAllEnabledGridcellsByCoords !< it returns the coordinates from the enabled gridcells
    procedure :: countAllEnabledGridcells !< it returns the number of enabled gridcells
    procedure :: setAllEnabledGridcells !< it defines a new list of enable gridcells
    procedure :: getAllEnabledGridcells !< it returns all enabled gridcells

    procedure :: setGridcellStatusByCoord !< it defines a new status for the given gridcell
                                          !< described as coordinate
    procedure :: setGridcellStatusByIdxs !< it defines a new status for the given gridcell 
                                         !< described as array indices form lats and lons
    generic :: setGridcellStatus => setGridcellStatusByCoord, setGridcellStatusByIdxs

    procedure :: getGloIndices
    procedure :: getCurrentPartitionId
    procedure :: getTotalPartitions
    procedure :: getPartitionBounds

    ! data related
    procedure :: transformDataSpatialToEnabled_rsp_2
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rsp_2 
    procedure :: transformDataSpatialToEnabled_rsp_3
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rsp_3 
    procedure :: transformDataSpatialToEnabled_rsp_4
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rsp_4 
    procedure :: transformDataSpatialToEnabled_rsp_5
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rsp_5 
    procedure :: transformDataSpatialToEnabled_rdp_2
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rdp_2 
    procedure :: transformDataSpatialToEnabled_rdp_3
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rdp_3 
    procedure :: transformDataSpatialToEnabled_rdp_4
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rdp_4 
    procedure :: transformDataSpatialToEnabled_rdp_5
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_rdp_5 
    procedure :: transformDataSpatialToEnabled_int_2
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_int_2 
    procedure :: transformDataSpatialToEnabled_int_3
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_int_3 
    procedure :: transformDataSpatialToEnabled_int_4
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_int_4 
    procedure :: transformDataSpatialToEnabled_int_5
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_int_5 

    procedure :: filterEnabledData_rsp_1
    generic :: filterEnabledData => filterEnabledData_rsp_1 
    procedure :: filterEnabledData_rsp_2
    generic :: filterEnabledData => filterEnabledData_rsp_2 
    procedure :: filterEnabledData_rsp_3
    generic :: filterEnabledData => filterEnabledData_rsp_3 
    procedure :: filterEnabledData_rsp_4
    generic :: filterEnabledData => filterEnabledData_rsp_4 
    procedure :: filterEnabledData_rdp_1
    generic :: filterEnabledData => filterEnabledData_rdp_1 
    procedure :: filterEnabledData_rdp_2
    generic :: filterEnabledData => filterEnabledData_rdp_2 
    procedure :: filterEnabledData_rdp_3
    generic :: filterEnabledData => filterEnabledData_rdp_3 
    procedure :: filterEnabledData_rdp_4
    generic :: filterEnabledData => filterEnabledData_rdp_4 
    procedure :: filterEnabledData_int_1
    generic :: filterEnabledData => filterEnabledData_int_1 
    procedure :: filterEnabledData_int_2
    generic :: filterEnabledData => filterEnabledData_int_2 
    procedure :: filterEnabledData_int_3
    generic :: filterEnabledData => filterEnabledData_int_3 
    procedure :: filterEnabledData_int_4
    generic :: filterEnabledData => filterEnabledData_int_4 

    ! private
    procedure, private :: coord2Index
    !procedure :: lat2Index
    !procedure :: lon2Index
  end type grid

  interface grid
    module procedure :: grid_constructor
  end interface

contains

  pure elemental logical function gridcell_eq(gc0,gc1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! equal to `d1` and `.false.` otherwise. Overloads the operator `==`.
    class(gridcell), intent(in) :: gc0, gc1

    logical :: sameIdx, sameCenter, sameRes, sameLimits, sameStatus

    sameIdx = (gc0 % idx == gc1 % idx) 
    sameCenter = ( gc0 % center == gc1 % center )
    sameRes = (gc0 % resolution == gc1 % resolution)
    sameLimits = ( gc0 % limits == gc1 % limits )
    sameStatus = ( gc0 % enabled .eqv. gc1 % enabled )

    res = sameIdx .and. sameCenter .and. sameRes .and. sameLimits .and. sameStatus
  end function gridcell_eq


  function getSpatialIdxs(self, nlons) result (idxs)
    !< it returns the spatial array indices from the current gridcell
    class(gridcell), intent(in) :: self
    integer, intent(in) :: nlons
    real(kind=sp) :: idxs(2) !< output (lat,lon)

    integer :: idxlat, idxlon

    idxlat = ((self % idx-1) / nlons) + 1
    idxlon = mod((self % idx-1), nlons) + 1

    idxs(1) = idxlat 
    idxs(2) = idxlon
        
  end function getSpatialIdxs


  real(kind=sp) function getEast(self)
    !< it returns the south latitude from the gridcell
    class(gridcell), intent(in) :: self

    getEast = self % limits % east
  end function getEast

  real(kind=sp) function getWest(self)
    !< it returns the south latitude from the gridcell
    class(gridcell), intent(in) :: self

    getWest = self % limits % west
  end function getWest

  real(kind=sp) function getSouth(self)
    !< it returns the south latitude from the gridcell
    class(gridcell), intent(in) :: self

    getSouth = self % limits % south
  end function getSouth

  real(kind=sp) function getNorth(self)
    !< it returns the north latitude from the gridcell
    class(gridcell), intent(in) :: self

    getNorth = self % limits % north
  end function getNorth


  function toString_gridcell(self) result (str)
    !< character representation of the gridcell class
    class(gridcell), intent(inout) :: self
    character(:), allocatable :: str
    character(100) :: idxStr, resStr
    write( idxStr, *) self % idx
    write( resStr, *) self % resolution

    str = "(" //trim(idxStr) // ") center=" // trim(self % center % toString()) // &
            ", res=" // trim(resStr) // &
            ", limits=" // trim(self % limits % toString()) !// self % enabled
  end function toString_gridcell


  logical function contains(self, c)
    !< true if the given coordinate 'c' fits the gridcell
    class(gridcell), intent(in) :: self
    type(coord), intent(in) :: c

    !write(*,*) "grid_mod::contains:: c=", c % toString()
    !write(*,*) "grid_mod::contains:: north, below north=", self % getNorth(), self % getNorth() >= c % lat
    !write(*,*) "grid_mod::contains:: north, above south=", self % getSouth(), self % getSouth() <= c % lat
    !write(*,*) "grid_mod::contains:: east, before east=", self % getEast(), self % getEast() >= c % lon
    !write(*,*) "grid_mod::contains:: west, after west=", self % getWest(), self % getWest() <= c % lon
    contains = .false.
    if (self % getNorth() >= c % lat .and. &
        self % getSouth() <= c % lat .and. &
        self % getEast() >= c % lon .and. &
        self % getWest() <= c % lon) then

      contains = .true.
    endif
    !write(*,*) "grid_mod::contains:: contains? ", contains
  end function contains


  type(gridcell) function gridcell_constructor(idx, resolution, center, enabled)
    !< constructs a new gridcell given the grid resolution and coordinate center of 
    !< the gridcell. 
    !< By default a gridcell is enabled. When enabled it's expect data from the variables
    integer, intent(in) :: idx !< gridcell index number in respect to the whole grid
    real(kind=sp), intent(in) :: resolution !< Grid's resolution. It helps to calculates
                                            !< gridcell's limits.
    type(coord), intent(in) :: center !< gridcell's center coordindate
    !type(pGridcell), intent(in), optional :: neighbours(GRIDCELL_NNEIGHS) !< 
    logical, intent(in), optional :: enabled !< gridcell is enabled (e.g: land)

    real(kind=sp) :: halfres

    halfres = resolution / 2.0

    gridcell_constructor % idx = idx
    gridcell_constructor % center = center
    gridcell_constructor % resolution = resolution
    gridcell_constructor % limits = window(center % lat + halfres, & ! north
                                     center % lat - halfres, & ! south
                                     center % lon + halfres, & ! east
                                     center % lon - halfres)  ! west
    gridcell_constructor % enabled = .true.
    if (present(enabled)) then
      gridcell_constructor % enabled = enabled
    endif

  end function gridcell_constructor


  type(grid) function grid_constructor(limits, resolution, currentPartition, totalPartitions, &
                  enabledGridcells)
    !< grid constructor
    !< in case current partition and total partitions are defined:
    !< - the domain will be divided into totalParitions parts
    !< - each part is defined by a unique currentPartition number
    !< - So the whole grid is divided with a number of equal enabled gridcells
    real(kind=sp), intent(in) :: limits(WINDOW_NCOORDS) !< north, south, east, west (90, -90, 180, -180)
    real(kind=sp), intent(in) :: resolution
    integer, intent(in) :: currentPartition !< current number of partition
    integer, intent(in) :: totalPartitions !< total number of parts the grid is divided into
    type(coord), intent(in), optional :: enabledGridcells(:) !< define which gridcells must be taken into account.
                                                             !< Partitioning will take into account only enabled gridcells
                                                             !< to balance them.

    call grid_constructor % constructor(limits, resolution, currentPartition, totalPartitions, enabledGridcells)
  end function grid_constructor


  subroutine oop_grid_constructor(self, limits, resolution, currentPartition, totalPartitions, enabledGridcells)
    !< grid constructor as a method
    !< in case current partition and total partitions are defined:
    !< - the domain will be divided into totalParitions parts
    !< - each part is defined by a unique currentPartition number
    !< - So the whole grid is divided with a number of equal enabled gridcells
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: limits(WINDOW_NCOORDS) !< north, south, east, west (90, -90, 180, -180)
    real(kind=sp), intent(in) :: resolution
    integer, intent(in) :: currentPartition !< current number of partition
    integer, intent(in) :: totalPartitions !< total number of parts the grid is divided into
    type(coord), intent(in), optional :: enabledGridcells(:) !< define which gridcells must be taken into account.
                                                             !< Partitioning will take into account only enabled gridcells   
                                                             !< to balance them.

    real(kind=sp) :: halfres
    real(kind=sp) :: start, end
    type(coord) :: ccentre

    integer :: nlats, nlons, ngridcells
    integer :: nidx, idxlat, idxlon

    logical :: hasNorth, hasSouth
    logical :: hasEast, hasWest
    integer :: idnorth, idnEast, idnWest
    integer :: idsouth, idsEast, idsWest

    integer, allocatable :: allIndices(:)

    ! todo check limits (above 90, n < s, ...)
    halfres = resolution / 2.

    ! generate lat lon -> coordinate points the center of the gridcell
    start = limits(WINDOW_NORTH) - halfres
    end = limits(WINDOW_SOUTH) + halfres
    self % lats = initArrayRange(start, end, -resolution)

    start = limits(WINDOW_EAST) - halfres
    end = limits(WINDOW_WEST) + halfres
    self % lons = initArrayRange(start, end, -resolution)

    call self % limits % init(limits)

    self % resolution = resolution

    ! initialize gridcells
    nlats = size(self % lats)
    nlons = size(self % lons)
    ngridcells = nlats * nlons
    nidx = 1
    allocate(self % gridcells(ngridcells))
    ! iterate the grid
    do idxlat = 1, nlats
      do idxlon = nlons, 1, -1
        ! create all gridcells
        ccentre = coord(self % lats(idxlat), self % lons(idxlon))
        self % gridcells(nidx) = gridcell(nidx, resolution, ccentre)
        nidx = nidx + 1 ! provide an unique number to each gridcell
      enddo
    enddo

    if (present(enabledGridcells)) then
      call self % setAllEnabledGridcells(enabledGridcells)
    endif

    allIndices = self % getLandIndices()

    allocate (self % partitions)
    self % partitions = grid_partition_type(currentPartition, totalPartitions, allIndices )
  end subroutine oop_grid_constructor


  logical function hasNeighbourByIdx(self, idxGC, direction)
    !< given a gridcell and a direction it returns true if there is one
    class(grid), intent(in) :: self
    integer, intent(in) :: idxGC !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    type(gridcell) :: gc

    gc = self % gridcells(idxGc)
    hasNeighbourByIdx = self % hasNeighbourByGc(gc, direction)
  end function hasNeighbourByIdx



  logical function hasNeighbourByGc(self, sourceGridcell, direction)
    !< given a gridcell and a direction it returns true if there is one
    class(grid), intent(in) :: self
    type(gridcell), intent(in) :: sourceGridcell !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    integer :: nidx !< continuous gridcell index (deal the array as only 1 dimension)
    integer :: idxlat, idxlon !< lat and lon array indices (array in 2 dimensions)

    ! available directions
    logical :: hasNorth, hasSouth
    logical :: hasEast, hasWest
    integer :: nlats !< total number of discretized latitudes
    integer :: nlons !< total number of discretized lontigues
    integer :: idxs(2)

    nlats = size (self % lats)
    nlons = size (self % lons)

    idxs = sourceGridcell % getSpatialIdxs(nlons)
    idxlat = idxs(1)
    idxlon = idxs(2)

    hasNorth = (idxlat > 1)
    hasSouth = (idxlat < nlats)

    hasWest = (idxlon > 1)
    hasEast = (idxlon < nlons)

    if (direction == GRIDCELL_N_NORTH) then ! has north?
      hasNeighbourByGc = hasNorth
    else if (direction == GRIDCELL_N_NEAST) then ! has north east
      hasNeighbourByGc = hasNorth .and. hasEast
    else if (direction == GRIDCELL_N_NWEST) then ! has north west
      hasNeighbourByGc = hasNorth .and. hasWest
    else if (direction == GRIDCELL_N_SOUTH) then ! has south
      hasNeighbourByGc = hasSouth
    else if (direction == GRIDCELL_N_SEAST) then ! has south east
      hasNeighbourByGc = hasSouth .and. hasEast
    else if (direction == GRIDCELL_N_SWEST) then ! has south west
      hasNeighbourByGc = hasSouth .and. hasWest
    else if (direction == GRIDCELL_N_EAST) then ! east 
      hasNeighbourByGc = hasEast
    else if (direction == GRIDCELL_N_WEST) then ! west
      hasNeighbourByGc = hasWest
    else
      ! unexpected error
      call raiseError(__FILE__, "hasNeighbourByGc", &
              "Unexpected direction found")
    endif
  end function hasNeighbourByGc


  function getNeighbourByIdx(self, gcIndex, direction) result (r)
    ! given a gridcell it returns its correspoding gridcell according to the 'direction' indicated
    class(grid), intent(in) :: self
    integer, intent(in) :: gcIndex !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    type(gridcell), allocatable :: r !< output

    r = self % getNeighbourByGc(self % gridcells(gcIndex), direction)
  end function getNeighbourByIdx


  function getNeighbourByGc(self, sGridcell, direction) result (r)
    ! given a gridcell it returns its correspoding gridcell according to the 'direction' indicated
    class(grid), intent(in) :: self
    type(gridcell), intent(in) :: sGridcell !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    type(gridcell), allocatable :: r !< output

    integer :: ngridcells
    integer :: idx !< continuous gridcell index (deal the array as only 1 dimension)
    integer :: idxlat, idxlon !< lat and lon array indices (array in 2 dimensions)

    ! calculated index
    integer :: idnorth, idnEast, idnWest
    integer :: idsouth, idsEast, idsWest
    integer :: idEast, idWest
    integer :: nlats !< total number of discretized latitudes
    integer :: nlons !< total number of discretized lontigues

    logical :: hasNeighb !< true is the requested direction has a neighbouring gridcell

    nlats = size(self % lats)
    nlons = size(self % lons)

    idx = sGridcell % idx
    hasNeighb = self % hasNeighbour(sGridcell, direction)
    !write(*,*) "grid_mod:: getNeighbourByGc:: gridcell index, has neighg?=", idx, hasNeighb

    if (.not. hasNeighb) then
      call raiseError(__FILE__, "getNeighbour", &
              "Requested direction has no neighbour")
    endif

    if (direction == GRIDCELL_N_NORTH) then ! has north?
      idnorth = idx - nlons ! calculate index for northen gridcell
      r = self % gridcells(idnorth)
    else if (direction == GRIDCELL_N_NEAST) then ! has north east
      idnEast = idx - nlons
      idnEast = idnEast + 1
      r = self % gridcells(idnEast)
    else if (direction == GRIDCELL_N_NWEST) then ! has north west
      idnWest = idx - nlons
      idnWest = idnWest - 1
      r = self % gridcells(idnWest)
    else if (direction == GRIDCELL_N_SOUTH) then ! has south
      idSouth = idx + nlons
      r = self % gridcells(idsouth)
    else if (direction == GRIDCELL_N_SEAST) then ! has south east
      idsEast = idx + nlons
      idsEast = idsEast + 1
      r = self % gridcells(idsEast)
    else if (direction == GRIDCELL_N_SWEST) then ! has south west
      idsWest = idx + nlons
      idsWest = idsWest - 1
      r = self % gridcells(idsWest)
    else if (direction == GRIDCELL_N_EAST) then ! has east
      idEast = idx + 1
      r = self % gridcells(idEast)
    else if (direction == GRIDCELL_N_WEST) then ! has west
      idWest = idx - 1
      r = self % gridcells(idWest)
    else
      ! unexpected error
      call raiseError(__FILE__, "getNeighbourByGc", &
              "Unexpected direction found")
    endif

  end function getNeighbourByGc


  function getSubGrid(self, newLimits, newResolution, currentPartition, totalPartitions) result (newGrid)
    !< it returns new grid with those given
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: newLimits(4) !< n, s, e, w
    real(kind=sp), intent(in), optional :: newResolution
    integer, intent(in), optional :: currentPartition 
    integer, intent(in), optional :: totalPartitions

    integer :: inCurrentPartition, inTotalPartitions

    real(kind=sp) :: res
    type(grid) :: newGrid

    if ( present(newResolution) ) then
      res = newResolution
    else
      res = self % getResolution()
    endif

    if (present(currentPartition) .and. present(totalPartitions) ) then
      inCurrentPartition = currentPartition
      inTotalPartitions = totalPartitions
    else
      inCurrentPartition = 0 
      inTotalPartitions = 1
    endif

    newGrid = grid(newLimits, res, inCurrentPartition, inTotalPartitions)
  end function getSubGrid


  logical elemental function fitsInCoord(self, nCoord) 
    !< true the given coordinate fits in the current one if true
    class(grid), intent(in) :: self
    type(coord), intent(in) :: nCoord
    fitsInCoord = self % limits % fitsCoord(nCoord)
  end function fitsInCoord


  logical elemental function fitsInGrid(self, smallerGrid) 
    !< true if the given grid fits in the current one if true
    class(grid), intent(in) :: self
    class(grid), intent(in) :: smallerGrid
    fitsInGrid = self % limits % fitsWindow(smallerGrid % limits)
  end function fitsInGrid


  logical elemental function fitsInBounds(self, bounds) 
    !< true if the given bounds fits in the current grid
    class(grid), intent(in) :: self
    type(window), intent(in) :: bounds
    fitsInBounds = self % limits % fitsWindow(bounds)
  end function fitsInBounds


  logical function isGlobal(self) 
    !< the current grid represent the whole globe
    class(grid), intent(inout) :: self
    real(kind=sp), parameter :: GLOBE(WINDOW_NCOORDS) = (/ 90,-90,180,-180 /)

    isGlobal = (self % limits == GLOBE)
  end function isGlobal


  function getLimits(self) result (limits)
    !< return the outter most latitudes and longitudes
    !<   north, south, east, west
    class(grid), intent(inout) :: self
    real(kind=sp) :: limits(WINDOW_NCOORDS) !< output

    limits = self % limits % toArray()
  end function getLimits


  function getResolution(self) result (resolution)
    class(grid), intent(inout) :: self
    real(sp) :: resolution

    resolution = self % resolution 
  end function getResolution


  function getLatCoords(self) result (allLats)
    !< return the latitude coordinates
    class(grid), intent(inout) :: self
    real(kind=sp), allocatable :: allLats(:) !< output

    allLats = self % lats
  end function getLatCoords


  function getLonCoords(self) result (allLons)
    !< return the longitude coordinates
    class(grid), intent(inout) :: self
    real(kind=sp), allocatable :: allLons(:) !< output

    allLons = self % lons
  end function getLonCoords


  function latCoord2Index(self, lat) result (idxs)
    !> return the indices of the gridcells where the coordinates match
    !> in case is in the middle of multiple gridcells, the corresponding
    !> indices are returned. At most 4 indices can be returned.
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: lat
    integer, allocatable :: idxs(:) !< output, indices found

    real(kind=sp), allocatable :: latCoords(:)

    latCoords = self % getLatCoords()
    idxs = self % coord2Index(lat, latCoords)

  end function latCoord2Index


  function lonCoord2Index(self, lon) result (idxs)
    !> it returns the indices where the 'lon' longitude value
    !> fits into the grid longitudes. 
    !> The outputs are the indices from its unique longitude array
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: lon
    integer, allocatable :: idxs(:) !< output, indices found

    real(kind=sp), allocatable :: lonCoords(:)

    lonCoords = self % getLonCoords()
    idxs = self % coord2Index(lon, lonCoords)

  end function lonCoord2Index


  function coord2Index(self, coord, array) result (indices)
    !> Given a unique coordinate and a set of coordinates
    !< it returns the indices of the corresponding 'array' gridcells
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: coord !< value to match
    real(kind=sp), intent(in) :: array(:) !< array coords to get index from
    integer, allocatable :: indices(:)

    integer, parameter :: MAX_IDXS = 2

    integer :: foundindices(MAX_IDXS)
    integer :: idxfirst, idxlast 
!
!    write(*,*) "grid_mod:: coord2Index:: coord, array =", coord, ", ", array
!
    idxfirst = closestNumber2Index(coord, array)
    idxlast = closestNumber2Index(coord, array, preferred=PREFER_LAST)

    foundIndices = (/ idxfirst, idxlast /)

    ! only unique numbers
    indices = unique(foundIndices)
  end function coord2Index


  function getLatSize(self) result (nlats)
    !< the total number of defined latitudes
    class(grid), intent(inout) :: self
    integer :: nlats !< output

    nlats = size (self % lats)
  end function getLatSize


  function getLonSize(self) result (nlons)
    !< the total number of defined latitudes
    class(grid), intent(inout) :: self
    integer :: nlons !< output

    nlons = size (self % lons)
  end function getLonSize

 
  function countAllEnabledGridcells(self, partition) result (counter)
    ! it returns an integer with the total number of enabled gridcells
    !< by default it returns all gridcells
    class(grid), intent(in) :: self
    integer, intent(in), optional :: partition
    integer :: counter !< output

    integer :: ix, inPartition
    integer :: bounds(2)
    integer :: startgcs, ngcs

    ! default as global
    startgcs = 1
    ngcs = size(self % gridcells)
    if (present(partition)) then ! chose partition bounds
      bounds = self % partitions % getPartitionBounds(partition)
      startgcs = bounds(1)
      ngcs = bounds(2)
    endif

    counter = 0
    do ix = startgcs, ngcs
      if (self % gridcells(ix) % enabled) then 
        counter = counter + 1
      endif
    enddo 
  end function countAllEnabledGridcells


  function getPartitionBounds(self, partition) result (bounds)
    !< it returns the requested 'partition' 1d bounds
    class(grid), intent(in) :: self
    integer, intent(in), optional :: partition
    integer :: bounds(2) !< output
    bounds = self % partitions % getPartitionBounds(partition)
  end function getPartitionBounds


  function getAllEnabledGridcellsByCoords(self, partition) result (cCoords)
    ! it returns array with all the enabled gridcells coordinates.
    ! those coordinates represent the center of the gridcell
    !< by default is global
    class(grid), intent(in) :: self
    integer, intent(in), optional :: partition !< global or partition id
    type(coord), allocatable :: cCoords(:) !< output

    type(gridcell), allocatable :: gcs(:)
    integer :: ix

    ! discover how many gridcells are enabled
    gcs = self % getAllEnabledGridcells(partition)

    ! place them in the output
    allocate(cCoords(size(gcs)))
    do ix = 1, size(gcs)
      cCoords(ix) = gcs(ix) % center
    enddo 

  end function getAllEnabledGridcellsByCoords


  function getAllEnabledGridcells(self, partition) result (gcs)
    ! it returns an array with all the enabled gridcells
    !< by default it returns all enabled gridcells 
    class(grid), intent(in) :: self
    integer, intent(in), optional :: partition
    type(gridcell), allocatable :: gcs(:)

    integer :: counter
    integer :: fidx !< found indices
    integer :: ix !< iterator
    integer :: startgcs, ngcs
    integer :: bounds(2)

    ! discover how many gridcells are enabled
    counter = self % countAllEnabledGridcells(partition)
!    write(*,*) "grid_mod::getAllEnabledGridcells:: enabled gc=", counter

    startgcs = 1 
    ngcs = size(self % gridcells)
    if (present(partition)) then
      bounds = self % partitions % getPartitionBounds(partition)
      startgcs = bounds(1)
      ngcs = bounds(2)
    endif

    ! place them in the output
    allocate(gcs(counter))
    fidx = 1
    do ix = startgcs, ngcs 
      if (self % gridcells(ix) % enabled) then 
        gcs(fidx) = self % gridcells(ix)
        fidx = fidx + 1
      endif
    enddo 

  end function getAllEnabledGridcells


  subroutine setGridcellStatusByIdxs(self, idxlat, idxlon, status)
    !< the corresponding gridcell from idxlat, idxlon define a new enabled status
    !< the gridcell position is calculated with idxlat and idxlon array indices
    class(grid), intent(inout) :: self
    integer, intent(in) :: idxlat, idxlon !< array indices from lats and lons
    logical, intent(in) :: status !< new gridcell status

    integer :: idx !< calculated gridcell index

    idx = ((idxlat-1) * size(self % lons)) + idxlon
    self %  gridcells(idx) % enabled = status

  end subroutine setGridcellStatusByIdxs    


  subroutine setGridcellStatusByCoord(self, newCoord, status)
    !< the corresponding gridcell from newCoord define a new enabled status
    class(grid), intent(inout) :: self
    type(coord), intent(in) :: newCoord !< 
    logical, intent(in) :: status !< 

    type(coord) :: gcCoord
    integer, allocatable :: idx(:)
    integer :: ix

    if (.not. self % fitsIn(newCoord)) then
      call raiseError(__FILE__, "setGridcellStatus", &
              "The given coordinate is not inside the grid", &
              "Coordinate(lat,lon):"//newCoord % toString(), &
              "Grid limits(n,s,e,w):"//self % limits % toString())
    endif

    ! only enable those provided by newCoods
!    write (*,*) "grid_mod::setAllEnabledGridcells:: seraching gc ", newCoord % toString() 

    ! search subroutine
    do ix = 1, size(self % gridcells)
      if (self % gridcells(ix) % contains(newCoord)) then 
        self % gridcells(ix) % enabled = status
        gcCoord = self %  gridcells(ix) % Center
!        write (*,*) "grid_mod::setridcellsStatus:: found gc coord=", gcCoord % toString()
!        write (*,*) "grid_mod::setridcellsStatus:: found gc idx=", self %  gridcells(ix) % idx
        exit 
      endif
    enddo 

  end subroutine setGridcellStatusByCoord


  subroutine setAllEnabledGridcells(self, newCoords) 
    !< it defines all enabled gridscells. Those not specified
    !< are set as disabled
    class(grid), intent(inout) :: self
    type(coord), intent(in), optional :: newCoords(:)

    logical, allocatable :: coordsOut(:)
    integer, allocatable :: idx(:)

    integer :: ix, xcoord

!    write(*,*) "grid_mod::setAllEnabledGridcells:: entering...", size(newCoords)

!    coordsOut = .not. self % fitsIn(newCoords)
!    if (any(coordsOut)) then
!      idx = findloc(coordsOut, .true.)

!      call raiseError(__FILE__, "setEnableGrids", &
!              "One of the given coordinates is not inside the grid", &
!              "Coordinate(lat,lon):"//newCoords(idx(1)) % toString(), &
!              "Grid limits(n,s,e,w):"//self % limits % toString())
!    endif

    ! set all gridcells to disabled
    do ix = 1, size(self % gridcells)
      self % gridcells(ix) % enabled = .false.
    enddo

    ! only enable those provided by newCoods
    if (present(newCoords)) then
      do xcoord = 1, size(newCoords)
!        write (*,*) "grid_mod::setAllEnabledGridcells:: seraching gc ", newCoords(xcoord) % toString() 

        call self % setGridcellStatus(newCoords(xCoord), .true.)
      enddo 
    endif
  end subroutine setAllEnabledGridcells


  logical function isCoordEnabled(self, newCoord) 
    !< true if the given coordinate is enabled
    class(grid), intent(inout) :: self
    type(coord), intent(in) :: newCoord
    character(len=*), parameter :: SNAME = "isCoordEnabled_scal"
    type(gridcell), allocatable :: gcs(:)

    gcs = self % getGridcellByCoord(newCoord)
    if (size(gcs) > 1) then
      call raiseError(__FILE__, "isCoordEnabled", &
              "Given coordinates are ambiguous", &
              "Multiple gridcells found but only 1 allowed")
    endif

    isCoordEnabled = gcs(1) % enabled
  end function isCoordEnabled



  function transformDataSpatialToEnabled_rsp_2(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:,:) !< lat, lon
    real(kind=sp), allocatable :: dout(:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_2"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx) = datain(gcidxs(2), gcIdxs(1) )
    enddo

  end function transformDataSpatialToEnabled_rsp_2


  function transformDataSpatialToEnabled_rsp_3(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:,:,:) !< lat, lon
    real(kind=sp), allocatable :: dout(:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_3"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:) = datain(gcidxs(2), gcIdxs(1) ,:)
    enddo

  end function transformDataSpatialToEnabled_rsp_3


  function transformDataSpatialToEnabled_rsp_4(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:,:,:,:) !< lat, lon
    real(kind=sp), allocatable :: dout(:,:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_4"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        , size(datain,dim=4) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:,:) = datain(gcidxs(2), gcIdxs(1) ,:,:)
    enddo

  end function transformDataSpatialToEnabled_rsp_4


  function transformDataSpatialToEnabled_rsp_5(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:,:,:,:,:) !< lat, lon
    real(kind=sp), allocatable :: dout(:,:,:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_5"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        , size(datain,dim=4) &
        , size(datain,dim=5) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:,:,:) = datain(gcidxs(2), gcIdxs(1) ,:,:,:)
    enddo

  end function transformDataSpatialToEnabled_rsp_5


  function transformDataSpatialToEnabled_rdp_2(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:,:) !< lat, lon
    real(kind=dp), allocatable :: dout(:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_2"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx) = datain(gcidxs(2), gcIdxs(1) )
    enddo

  end function transformDataSpatialToEnabled_rdp_2


  function transformDataSpatialToEnabled_rdp_3(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:,:,:) !< lat, lon
    real(kind=dp), allocatable :: dout(:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_3"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:) = datain(gcidxs(2), gcIdxs(1) ,:)
    enddo

  end function transformDataSpatialToEnabled_rdp_3


  function transformDataSpatialToEnabled_rdp_4(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:,:,:,:) !< lat, lon
    real(kind=dp), allocatable :: dout(:,:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_4"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        , size(datain,dim=4) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:,:) = datain(gcidxs(2), gcIdxs(1) ,:,:)
    enddo

  end function transformDataSpatialToEnabled_rdp_4


  function transformDataSpatialToEnabled_rdp_5(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:,:,:,:,:) !< lat, lon
    real(kind=dp), allocatable :: dout(:,:,:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_5"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        , size(datain,dim=4) &
        , size(datain,dim=5) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:,:,:) = datain(gcidxs(2), gcIdxs(1) ,:,:,:)
    enddo

  end function transformDataSpatialToEnabled_rdp_5


  function transformDataSpatialToEnabled_int_2(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:,:) !< lat, lon
    integer, allocatable :: dout(:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_2"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx) = datain(gcidxs(2), gcIdxs(1) )
    enddo

  end function transformDataSpatialToEnabled_int_2


  function transformDataSpatialToEnabled_int_3(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:,:,:) !< lat, lon
    integer, allocatable :: dout(:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_3"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:) = datain(gcidxs(2), gcIdxs(1) ,:)
    enddo

  end function transformDataSpatialToEnabled_int_3


  function transformDataSpatialToEnabled_int_4(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:,:,:,:) !< lat, lon
    integer, allocatable :: dout(:,:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_4"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        , size(datain,dim=4) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:,:) = datain(gcidxs(2), gcIdxs(1) ,:,:)
    enddo

  end function transformDataSpatialToEnabled_int_4


  function transformDataSpatialToEnabled_int_5(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:,:,:,:,:) !< lat, lon
    integer, allocatable :: dout(:,:,:,:)

    type(gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_5"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. size(self % lats) .or. nlons .ne. size(self % lons)) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
        , size(datain,dim=3) &
        , size(datain,dim=4) &
        , size(datain,dim=5) &
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx,:,:,:) = datain(gcidxs(2), gcIdxs(1) ,:,:,:)
    enddo

  end function transformDataSpatialToEnabled_int_5



      
  function filterEnabledData_rsp_1(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=sp), allocatable :: dout(:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_1"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ) = datain(idx )
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rsp_1

      
  function filterEnabledData_rsp_2(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=sp), allocatable :: dout(:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_2"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:) = datain(idx ,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rsp_2

      
  function filterEnabledData_rsp_3(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:,:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=sp), allocatable :: dout(:,:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_3"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                , size(datain, dim=3) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:,:) = datain(idx ,:,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rsp_3

      
  function filterEnabledData_rsp_4(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=sp), intent(in) :: datain(:,:,:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=sp), allocatable :: dout(:,:,:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rsp_4"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                , size(datain, dim=3) &
                , size(datain, dim=4) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:,:,:) = datain(idx ,:,:,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rsp_4

      
  function filterEnabledData_rdp_1(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=dp), allocatable :: dout(:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_1"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ) = datain(idx )
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rdp_1

      
  function filterEnabledData_rdp_2(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=dp), allocatable :: dout(:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_2"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:) = datain(idx ,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rdp_2

      
  function filterEnabledData_rdp_3(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:,:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=dp), allocatable :: dout(:,:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_3"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                , size(datain, dim=3) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:,:) = datain(idx ,:,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rdp_3

      
  function filterEnabledData_rdp_4(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    real(kind=dp), intent(in) :: datain(:,:,:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    real(kind=dp), allocatable :: dout(:,:,:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_rdp_4"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                , size(datain, dim=3) &
                , size(datain, dim=4) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:,:,:) = datain(idx ,:,:,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_rdp_4

      
  function filterEnabledData_int_1(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    integer, allocatable :: dout(:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_1"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ) = datain(idx )
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_int_1

      
  function filterEnabledData_int_2(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    integer, allocatable :: dout(:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_2"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:) = datain(idx ,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_int_2

      
  function filterEnabledData_int_3(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:,:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    integer, allocatable :: dout(:,:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_3"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                , size(datain, dim=3) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:,:) = datain(idx ,:,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_int_3

      
  function filterEnabledData_int_4(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(grid), intent(inout) :: self
    integer, intent(in) :: datain(:,:,:,:) !< global variable
    type(coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    integer, allocatable :: dout(:,:,:,:) !< output

    type(gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_int_4"

    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs)
    ! ensure datain has the same size as enabled gridcells
    if (size(datain, dim=1) .ne. size(enabledGcs)) then
      write(tmp,*) size(enabledGcs)
      write(tmp1,*) size(datain, dim=1) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as enabled gridcells", &
               "Enabled gridcells: "//tmp, &
               "Array spatial size: "//tmp1)
    endif

    ! find all gridcells from coords
    allocate(gcs(size(coords)))
    do i = 1, size(coords)
      ! procided gridcell can only be inside the gridcell
      ! so it is only allowed 1 gridcell as output
      tmpGc = self % getGridcellByCoord(coords(i))
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: coords found=", coords(i), tmpGc(1) % idx 
      if (size(tmpGc) .ne. 1) then
        call raiseError(__FILE__, SNAME, "More than a gridcell", &
                "Coordinates give: "//coords(i) % toString())
      endif
      gcs(i) = tmpGc(1)
!      write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc, i=", gcs(i) % idx, i
    enddo

    ! filter selected gridcells
    allocate(dout(size(coords) &! ,size(datain,dim=2)))
                , size(datain, dim=2) &
                , size(datain, dim=3) &
                , size(datain, dim=4) &
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ,:,:,:) = datain(idx ,:,:,:)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_int_4



  function getGridcellBySpatialIndices(self, latx, lonx) result (foundGc)
    !< return gridcell(s) given a coordinate
    class(grid), intent(inout) :: self
    integer, intent(in) :: latx, lonx
    type(gridcell) :: foundGc !< output

    integer :: idx
    real(kind=sp) :: clat, clon
    type(coord) :: newC
    type(gridcell), allocatable :: gcs(:)
    type(gridcell) :: oldGC

!    write(*,*) "grid_mod::getGridcellBySpatialIndices:: latx, lonx =", latx, lonx

    ! old
!    clat = self % lats(latx)
!    clon = self % lons(lonx)
!    newC = coord(clat, clon)
!    gcs = self % getGridcellByCoord(newC)

    ! calculate gridcell index from lat and lon array indices
    idx = ((latx-1) * size(self % lons)) + lonx

    foundGc =  self % gridcells(idx)!gcs(1)

!    write(*,*) "grid_mod::getGridcellBySpatialIndices:: found vs old?", foundGc == gcs(1)
!    write(*,*) "grid_mod::getGridcellBySpatialIndices:: idx(found vs old):", foundGc % idx, gcs(1) % idx

  end function getGridcellBySpatialIndices


  function getGridcellByCoord(self, c) result (r)
    !< return gridcell(s) given a coordinate
    class(grid), intent(inout) :: self
    type(coord), intent(in) :: c
    type(gridcell), allocatable :: r(:) !< output

    !integer :: nlats, nlons
    integer :: idx !, xlat, xlon, idx
    type(gridcell) :: gc

    if (.not. self % fitsIn(c)) then
      call raiseError(__FILE__, "getGridcellByCoord", &
              "The given coordinate is not inside the current grid", &
              "Grid limits(n,s,e,w):"//self % limits % toString(), &
              "Coordinate(lat, lon): "//c % toString())
    endif

    !nlats = size(self % lats)
    !nlons = size(self % lons)
    idx = 1

    ! empty initialization
    allocate(r(0))

    do idx = 1, size(self % gridcells)
      gc = self % gridcells(idx)
!      write (*,*) "grid_mod::getGridcellByCoord:: searching coordinate ", c % toString(), " ..."
!      write (*,*) "grid_mod::getGridcellByCoord:: idx, contains=", idx, gc % idx
!      write (*,*) "grid_mod::getGridcellByCoord:: limits=", gc % limits % toString()
      if (gc % contains(c)) then ! found!
!        write (*,*) "grid_mod::getGridcellByCoord:: found!"
        ! merge with previous elements
        r = [r, gc]
      endif
    enddo
  end function getGridcellByCoord


  function getGloIndices(self) result (allidx)
    !< return all indices from the grid
    !< each index is a unique number assigned to each gridcell 
    class(grid), intent(inout) :: self
    integer, allocatable :: allidx(:)
    integer :: idx 
    integer :: ngcs !< total number of gridcells
  
    ngcs = self % countAllGridcells()  
    if (allocated(allidx)) deallocate(allidx)
    allocate(allidx(ngcs))

    do idx = 1, size(self % gridcells)
      allidx(idx) = self % gridcells(idx) % idx 
    enddo
  end function getGloIndices


  function countAllGridcells(self) result (ngcs)
    !< return the total number of gridcells in the grid 
    class(grid), intent(inout) :: self
    integer :: ngcs !< intent(out )
    integer :: idx

    ngcs = 0
    do idx = 1, size(self % gridcells)
      ngcs = ngcs + 1
    enddo
  end function countAllGridcells


  function getLandBounds(self, partition) result (bounds)
    !< global or loca, get gridcell indices from requested domain
    !< domain: global or local domains, from 0 to P-1. Use -1 for global domain
    class(grid), intent(in) :: self
    integer, intent(in), optional :: partition
    integer :: bounds(2)
    bounds = self % partitions % getPartitionBounds(partition)
  end function getLandBounds


  function getLandIndices(self, partition) result (landIndices)
    !< global / local, get gridcell indices from requested domain
    !< domain: global or local domains, from 0 to P-1. Use -1 for global domain
    class(grid), intent(in) :: self
    integer, intent(in), optional :: partition
    integer, allocatable :: landIndices(:)
    type(gridcell), allocatable :: enabledGCs(:) !< enabled gridcells found in 'coords'
    integer :: igc, ngcs
    if (present(partition)) then ! local (partition)
      landIndices = self % partitions % getPartitionIndices(partition)
    else !global
      enabledGCs = self % getAllEnabledGridcells(partition)
      ngcs = size(enabledGCs)
      if (allocated(landIndices)) deallocate(landIndices)
      allocate(landIndices(ngcs))

      do igc=1, ngcs 
        landIndices(igc) = enabledGCs(igc) % idx
      enddo
    endif
  end function getLandIndices


  integer function getCurrentPartitionId(self)
    !< it returns the current partition id
    class(grid), intent(in) :: self
    getCurrentPartitionId = self % partitions % getPartitionId()
  end function getCurrentPartitionId


  integer function getTotalPartitions(self)
    !< it returns the current partition id
    class(grid), intent(in) :: self
    getTotalPartitions = self % partitions % getTotalPartitions()
  end function getTotalPartitions


end module shr_grid_mod
