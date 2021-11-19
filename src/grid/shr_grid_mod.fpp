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

#:include "../common.fpp"

module shr_grid_mod

  use SHR_precision_mod, only: dp, sp 
  use SHR_error_mod, only: raiseError
  use shr_arrayDim_mod, only: shr_arrayRspDim
  use SHR_arrayUtils_mod, only: unique, closestNumber2Index, PREFER_LAST, &
                        initArrayRange
  use shr_coord_mod, only: shr_coord
  use shr_gridPartition_mod, only: shr_gridPartition
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gridBounds_mod, only: SHR_GRIDBOUNDS_NCOORDS, SHR_GRIDBOUNDS_NORTH, &
             SHR_GRIDBOUNDS_SOUTH, SHR_GRIDBOUNDS_EAST, SHR_GRIDBOUNDS_WEST

  use shr_gridcell_mod, only: shr_gridcell
  use shr_gridcell_mod, only: GRIDCELL_NNEIGHS, GRIDCELL_N_NEAST, GRIDCELL_N_NORTH, GRIDCELL_N_EAST, &
            GRIDCELL_N_SEAST, GRIDCELL_N_SOUTH, GRIDCELL_N_SWEST, GRIDCELL_N_WEST, &
            GRIDCELL_N_NWEST

  implicit none

  private

  public :: shr_grid


  type :: shr_grid !< latitude and longitude have the same resolution
    class(shr_arrayRspDim), allocatable :: lats
    class(shr_arrayRspDim), allocatable :: lons !< list all longitudes
    real(kind=sp) :: resolution !< grid resolution
    type(shr_gridBounds) :: limits !< max/min lats/lons of the grid

    type(shr_gridcell), allocatable :: gridcells(:) !< gridcells
    type(shr_gridPartition), allocatable :: partitions
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
#:for _, _, IHEADER  in ALL_KINDS_TYPES
  #:for RANK in RSHIFTRANKS
    procedure :: transformDataSpatialToEnabled_${IHEADER}$_${RANK}$
    generic :: transformDataSpatialToEnabled => transformDataSpatialToEnabled_${IHEADER}$_${RANK}$ 
  #:endfor
#:endfor

#:for _, _, IHEADER  in ALL_KINDS_TYPES
  #:for RANK in RANKS
    procedure :: filterEnabledData_${IHEADER}$_${RANK}$
    generic :: filterEnabledData => filterEnabledData_${IHEADER}$_${RANK}$ 
  #:endfor
#:endfor

    ! private
    procedure, private :: coord2Index
    !procedure :: lat2Index
    !procedure :: lon2Index
  end type shr_grid

  interface shr_grid
    module procedure :: grid_constructor
  end interface

contains


  type(shr_grid) function grid_constructor(limits, resolution, currentPartition, totalPartitions, &
                  enabledGridcells)
    !< grid constructor
    !< in case current partition and total partitions are defined:
    !< - the domain will be divided into totalParitions parts
    !< - each part is defined by a unique currentPartition number
    !< - So the whole grid is divided with a number of equal enabled gridcells
    real(kind=sp), intent(in) :: limits(SHR_GRIDBOUNDS_NCOORDS) !< north, south, east, west (90, -90, 180, -180)
    real(kind=sp), intent(in) :: resolution
    integer, intent(in) :: currentPartition !< current number of partition
    integer, intent(in) :: totalPartitions !< total number of parts the grid is divided into
    type(shr_coord), intent(in), optional :: enabledGridcells(:) !< define which gridcells must be taken into account.
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
    class(shr_grid), intent(inout) :: self
    real(kind=sp), intent(in) :: limits(SHR_GRIDBOUNDS_NCOORDS) !< north, south, east, west (90, -90, 180, -180)
    real(kind=sp), intent(in) :: resolution
    integer, intent(in) :: currentPartition !< current number of partition
    integer, intent(in) :: totalPartitions !< total number of parts the grid is divided into
    type(shr_coord), intent(in), optional :: enabledGridcells(:) !< define which gridcells must be taken into account.
                                                             !< Partitioning will take into account only enabled gridcells   
                                                             !< to balance them.

    real(kind=sp) :: halfres
    real(kind=sp) :: start, end
    type(shr_coord) :: ccentre

    integer :: nlats, nlons, ngridcells
    integer :: nidx, idxlat, idxlon

    logical :: hasNorth, hasSouth
    logical :: hasEast, hasWest
    integer :: idnorth, idnEast, idnWest
    integer :: idsouth, idsEast, idsWest

    integer, allocatable :: allIndices(:)
    real(kind=sp) :: latVal, lonVal

    ! todo check limits (above 90, n < s, ...)
    halfres = resolution / 2.

    ! generate lat lon -> coordinate points the center of the gridcell
    start = limits(SHR_GRIDBOUNDS_NORTH) - halfres
    end = limits(SHR_GRIDBOUNDS_SOUTH) + halfres
    allocate(self % lats)
    call self % lats % init("latitude", start, end, -resolution)


    start = limits(SHR_GRIDBOUNDS_EAST) - halfres
    end = limits(SHR_GRIDBOUNDS_WEST) + halfres
    allocate(self % lons)
    call self % lons % init("longitude", start, end, -resolution)

    call self % limits % init(limits)

    self % resolution = resolution

    ! initialize gridcells
    nlats = self % lats % getSize()
    nlons = self % lons % getSize()
    ngridcells = nlats * nlons
    nidx = 1
    allocate(self % gridcells(ngridcells))
    ! iterate the grid
    do idxlat = 1, nlats
      do idxlon = nlons, 1, -1
        ! create all gridcells
        latVal = self % lats % getValue(idxlat)
        lonVal = self % lons % getValue(idxlon)
        ccentre = shr_coord(latVal, lonVal)
        self % gridcells(nidx) = shr_gridcell(nidx, resolution, ccentre)
        nidx = nidx + 1 ! provide an unique number to each gridcell
      enddo
    enddo

    if (present(enabledGridcells)) then
      call self % setAllEnabledGridcells(enabledGridcells)
    endif

    allIndices = self % getLandIndices()

    allocate (self % partitions)
    self % partitions = shr_gridPartition(currentPartition, totalPartitions, allIndices )
  end subroutine oop_grid_constructor


  logical function hasNeighbourByIdx(self, idxGC, direction)
    !< given a gridcell and a direction it returns true if there is one
    class(shr_grid), intent(in) :: self
    integer, intent(in) :: idxGC !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    type(shr_gridcell) :: gc

    gc = self % gridcells(idxGc)
    hasNeighbourByIdx = self % hasNeighbourByGc(gc, direction)
  end function hasNeighbourByIdx



  logical function hasNeighbourByGc(self, sourceGridcell, direction)
    !< given a gridcell and a direction it returns true if there is one
    class(shr_grid), intent(in) :: self
    type(shr_gridcell), intent(in) :: sourceGridcell !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    integer :: nidx !< continuous gridcell index (deal the array as only 1 dimension)
    integer :: idxlat, idxlon !< lat and lon array indices (array in 2 dimensions)

    ! available directions
    logical :: hasNorth, hasSouth
    logical :: hasEast, hasWest
    integer :: nlats !< total number of discretized latitudes
    integer :: nlons !< total number of discretized lontigues
    integer :: idxs(2)

    nlats = self % lats % getSize()
    nlons = self % lons % getSize()

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
    class(shr_grid), intent(in) :: self
    integer, intent(in) :: gcIndex !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    type(shr_gridcell), allocatable :: r !< output

    r = self % getNeighbourByGc(self % gridcells(gcIndex), direction)
  end function getNeighbourByIdx


  function getNeighbourByGc(self, sGridcell, direction) result (r)
    ! given a gridcell it returns its correspoding gridcell according to the 'direction' indicated
    class(shr_grid), intent(in) :: self
    type(shr_gridcell), intent(in) :: sGridcell !< from which gridcell to request its neighbour
    integer, intent(in) :: direction !< neighbour's direction

    type(shr_gridcell), allocatable :: r !< output

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

    nlats = self % lats % getSize()
    nlons = self % lons % getSize()

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
    class(shr_grid), intent(in) :: self
    real(kind=sp), intent(in) :: newLimits(4) !< n, s, e, w
    real(kind=sp), intent(in), optional :: newResolution
    integer, intent(in), optional :: currentPartition 
    integer, intent(in), optional :: totalPartitions

    integer :: inCurrentPartition, inTotalPartitions

    real(kind=sp) :: res
    type(shr_grid) :: newGrid

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

    newGrid = shr_grid(newLimits, res, inCurrentPartition, inTotalPartitions)
  end function getSubGrid


  logical elemental function fitsInCoord(self, nCoord) 
    !< true the given coordinate fits in the current one if true
    class(shr_grid), intent(in) :: self
    type(shr_coord), intent(in) :: nCoord
    fitsInCoord = self % limits % fitsCoord(nCoord)
  end function fitsInCoord


  logical elemental function fitsInGrid(self, smallerGrid) 
    !< true if the given grid fits in the current one if true
    class(shr_grid), intent(in) :: self
    class(shr_grid), intent(in) :: smallerGrid
    fitsInGrid = self % limits % fitsGridBounds(smallerGrid % limits)
  end function fitsInGrid


  logical elemental function fitsInBounds(self, bounds) 
    !< true if the given bounds fits in the current grid
    class(shr_grid), intent(in) :: self
    type(shr_gridBounds), intent(in) :: bounds
    fitsInBounds = self % limits % fitsGridBounds(bounds)
  end function fitsInBounds


  logical function isGlobal(self) 
    !< the current grid represent the whole globe
    class(shr_grid), intent(inout) :: self
    real(kind=sp), parameter :: GLOBE(SHR_GRIDBOUNDS_NCOORDS) = (/ 90,-90,180,-180 /)

    isGlobal = (self % limits == GLOBE)
  end function isGlobal


  function getLimits(self) result (limits)
    !< return the outter most latitudes and longitudes
    !<   north, south, east, west
    class(shr_grid), intent(in) :: self
    real(kind=sp) :: limits(SHR_GRIDBOUNDS_NCOORDS) !< output

    limits = self % limits % toArray()
  end function getLimits


  function getResolution(self) result (resolution)
    class(shr_grid), intent(in) :: self
    real(sp) :: resolution

    resolution = self % resolution 
  end function getResolution


  function getLatCoords(self) result (allLats)
    !< return the latitude coordinates
    class(shr_grid), intent(inout) :: self
    real(kind=sp), allocatable :: allLats(:) !< output
    allLats = self % lats % getAllValues()
  end function getLatCoords


  function getLonCoords(self) result (allLons)
    !< return the longitude coordinates
    class(shr_grid), intent(inout) :: self
    real(kind=sp), allocatable :: allLons(:) !< output
    allLons = self % lons % getAllValues()
  end function getLonCoords


  function latCoord2Index(self, lat) result (idxs)
    !> return the indices of the gridcells where the coordinates match
    !> in case is in the middle of multiple gridcells, the corresponding
    !> indices are returned. At most 4 indices can be returned.
    class(shr_grid), intent(inout) :: self
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
    class(shr_grid), intent(inout) :: self
    real(kind=sp), intent(in) :: lon
    integer, allocatable :: idxs(:) !< output, indices found

    real(kind=sp), allocatable :: lonCoords(:)

    lonCoords = self % getLonCoords()
    idxs = self % coord2Index(lon, lonCoords)

  end function lonCoord2Index


  function coord2Index(self, coord, array) result (indices)
    !> Given a unique coordinate and a set of coordinates
    !< it returns the indices of the corresponding 'array' gridcells
    class(shr_grid), intent(inout) :: self
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
    class(shr_grid), intent(in) :: self
    integer :: nlats !< output
    nlats = self % lats % getSize()
  end function getLatSize


  function getLonSize(self) result (nlons)
    !< the total number of defined latitudes
    class(shr_grid), intent(in) :: self
    integer :: nlons !< output
    nlons = self % lons % getSize()
  end function getLonSize

 
  function countAllEnabledGridcells(self, partition) result (counter)
    ! it returns an integer with the total number of enabled gridcells
    !< by default it returns all gridcells
    class(shr_grid), intent(in) :: self
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
    class(shr_grid), intent(in) :: self
    integer, intent(in), optional :: partition
    integer :: bounds(2) !< output
    bounds = self % partitions % getPartitionBounds(partition)
  end function getPartitionBounds


  function getAllEnabledGridcellsByCoords(self, partition) result (cCoords)
    ! it returns array with all the enabled gridcells coordinates.
    ! those coordinates represent the center of the gridcell
    !< by default is global
    class(shr_grid), intent(in) :: self
    integer, intent(in), optional :: partition !< global or partition id
    type(shr_coord), allocatable :: cCoords(:) !< output

    type(shr_gridcell), allocatable :: gcs(:)
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
    class(shr_grid), intent(in) :: self
    integer, intent(in), optional :: partition
    type(shr_gridcell), allocatable :: gcs(:)

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
    class(shr_grid), intent(inout) :: self
    integer, intent(in) :: idxlat, idxlon !< array indices from lats and lons
    logical, intent(in) :: status !< new gridcell status

    integer :: idx !< calculated gridcell index

    idx = ((idxlat-1) * self % lons % getSize()) + idxlon
    self %  gridcells(idx) % enabled = status

  end subroutine setGridcellStatusByIdxs    


  subroutine setGridcellStatusByCoord(self, newCoord, status)
    !< the corresponding gridcell from newCoord define a new enabled status
    class(shr_grid), intent(inout) :: self
    type(shr_coord), intent(in) :: newCoord !< 
    logical, intent(in) :: status !< 

    type(shr_coord) :: gcCoord
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
    class(shr_grid), intent(inout) :: self
    type(shr_coord), intent(in), optional :: newCoords(:)

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
    class(shr_grid), intent(inout) :: self
    type(shr_coord), intent(in) :: newCoord
    character(len=*), parameter :: SNAME = "isCoordEnabled_scal"
    type(shr_gridcell), allocatable :: gcs(:)

    gcs = self % getGridcellByCoord(newCoord)
    if (size(gcs) > 1) then
      call raiseError(__FILE__, "isCoordEnabled", &
              "Given coordinates are ambiguous", &
              "Multiple gridcells found but only 1 allowed")
    endif

    isCoordEnabled = gcs(1) % enabled
  end function isCoordEnabled


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  #:for RANK in RSHIFTRANKS

  function transformDataSpatialToEnabled_${IHEADER}$_${RANK}$(self, datain) result (dout)
    !< it transform a full spatial(2d) array into a unique enabled array (1d)
    !< 'datain' must have the same size as the grid
    !< the other non spatial dimensions are kept 
    class(shr_grid), intent(inout) :: self
    ${ITYPE}$, intent(in) :: datain${ranksuffix(RANK)}$ !< lat, lon
    ${ITYPE}$, allocatable :: dout${ranksuffix(RANK-1)}$

    type(shr_gridcell), allocatable :: enabledGcs(:)

    integer :: nlats, nlons !< latitude and longitude size
    integer :: ngcs !< number of gridcells
    integer :: gcidxs(2)
    integer :: idx
    character(len=:), allocatable :: strSize
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_${IHEADER}$_${RANK}$"

    nlats = size(datain, dim=1)
    nlons = size(datain, dim=2)
    enabledGcs = self % getAllEnabledGridcells()
    ngcs = size(enabledGcs) 

    !< same dimensions for datain vs grid?
    if (nlats .ne. self % lats % getSize() .or. nlons .ne. self % lons % getSize()) then
      write(strSize,*) size(datain) 
      call raiseError(__FILE__, SNAME, &
              "The given array does not have the same size as the grid", &
              "Grid size: ", self % limits % toString(), &
              "Array size:"//strSize)
    endif

    allocate(dout(ngcs &
  #:for IRANK in range(3,int(RANK+1))
        , size(datain,dim=${IRANK}$) &
  #:endfor
        ))

!    write(*,*) "grid_mod::"//SNAME//":: total enabled grids=", ngcs 
!    write(*,*) "grid_mod::"//SNAME//":: data in size 1st =", size(datain, dim=1)
!    write(*,*) "grid_mod::"//SNAME//":: data in size 2nd =", size(datain, dim=2)

    ! iterate data
    do idx = 1, ngcs
!      write(*,*) "grid_mod::"// SNAME //":: indices=", idx, enabledGcs(idx) % idx
      
      gcidxs = enabledGcs(idx) % getSpatialIdxs(nlons)
!      write(*,*) "grid_mod::"// SNAME //":: ", idx, " <--- ", gcidxs(2), gcIdxs(1)
      dout(idx${rankimplicit(RANK-2)}$) = datain(gcidxs(2), gcIdxs(1) ${rankimplicit(RANK-2)}$)
    enddo

  end function transformDataSpatialToEnabled_${IHEADER}$_${RANK}$

  #:endfor
#:endfor


#:for IKIND, ITYPE, IHEADER  in ALL_KINDS_TYPES
  #:for RANK in RANKS
      
  function filterEnabledData_${IHEADER}$_${RANK}$(self, datain, coords) result (dout)
    ! filter gridcells from the given array
    ! the given array must be 1d spatial and the grid size
    class(shr_grid), intent(inout) :: self
    ${ITYPE}$, intent(in) :: datain${ranksuffix(RANK)}$ !< global variable
    type(shr_coord), intent(in) :: coords(:) !< gridcell's coordinates to filter in 
    ${ITYPE}$, allocatable :: dout${ranksuffix(RANK)}$ !< output

    type(shr_gridcell), allocatable :: tmpGc(:) !< gridcell found in 'coords'
    type(shr_gridcell), allocatable :: gcs(:) !< gridcell found in 'coords'
    type(shr_gridcell), allocatable :: enabledGcs(:) !< enabled gridcells found in 'coords'
    integer :: idx, i, cidx, counter !< iterator
    integer :: ngcs
    character(len=50) :: tmp, tmp1
    character(*), parameter :: SNAME = "transformDataSpatialToEnabled_${IHEADER}$_${RANK}$"

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
    #:for IRANK in range(2, int(RANK+1))
                , size(datain, dim=${IRANK}$) &
    #:endfor
                ))

    counter = 1
    do idx = 1, ngcs 
      do cidx = 1, size(gcs)
        if (enabledGcs(idx) == gcs(cidx)) then ! found
!          write(*,*) "grid_mod::filterEnabledData_sp_r2:: gc idx found=", gcs(cidx) % idx 
          dout(counter ${rankimplicit(RANK-1)}$) = datain(idx ${rankimplicit(RANK-1)}$)
          counter = counter + 1
        endif
      enddo
    enddo

  end function filterEnabledData_${IHEADER}$_${RANK}$

  #:endfor
#:endfor


  function getGridcellBySpatialIndices(self, latx, lonx) result (foundGc)
    !< return gridcell(s) given a coordinate
    class(shr_grid), intent(inout) :: self
    integer, intent(in) :: latx, lonx
    type(shr_gridcell) :: foundGc !< output

    integer :: idx
    real(kind=sp) :: clat, clon
    type(shr_coord) :: newC
    type(shr_gridcell), allocatable :: gcs(:)
    type(shr_gridcell) :: oldGC

!    write(*,*) "grid_mod::getGridcellBySpatialIndices:: latx, lonx =", latx, lonx

    ! old
!    clat = self % lats(latx)
!    clon = self % lons(lonx)
!    newC = shr_coord(clat, clon)
!    gcs = self % getGridcellByCoord(newC)

    ! calculate gridcell index from lat and lon array indices
    idx = ((latx-1) * self % lons % getSize()) + lonx

    foundGc =  self % gridcells(idx)!gcs(1)

!    write(*,*) "grid_mod::getGridcellBySpatialIndices:: found vs old?", foundGc == gcs(1)
!    write(*,*) "grid_mod::getGridcellBySpatialIndices:: idx(found vs old):", foundGc % idx, gcs(1) % idx

  end function getGridcellBySpatialIndices


  function getGridcellByCoord(self, c) result (r)
    !< return gridcell(s) given a coordinate
    class(shr_grid), intent(inout) :: self
    type(shr_coord), intent(in) :: c
    type(shr_gridcell), allocatable :: r(:) !< output

    !integer :: nlats, nlons
    integer :: idx !, xlat, xlon, idx
    type(shr_gridcell) :: gc

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
    class(shr_grid), intent(inout) :: self
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
    class(shr_grid), intent(inout) :: self
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
    class(shr_grid), intent(in) :: self
    integer, intent(in), optional :: partition
    integer :: bounds(2)
    bounds = self % partitions % getPartitionBounds(partition)
  end function getLandBounds


  function getLandIndices(self, partition) result (landIndices)
    !< global / local, get gridcell indices from requested domain
    !< domain: global or local domains, from 0 to P-1. Use -1 for global domain
    class(shr_grid), intent(in) :: self
    integer, intent(in), optional :: partition
    integer, allocatable :: landIndices(:)
    type(shr_gridcell), allocatable :: enabledGCs(:) !< enabled gridcells found in 'coords'
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
    class(shr_grid), intent(in) :: self
    getCurrentPartitionId = self % partitions % getPartitionId()
  end function getCurrentPartitionId


  integer function getTotalPartitions(self)
    !< it returns the current partition id
    class(shr_grid), intent(in) :: self
    getTotalPartitions = self % partitions % getTotalPartitions()
  end function getTotalPartitions


end module shr_grid_mod
