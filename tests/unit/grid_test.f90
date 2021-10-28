!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : gridPartition_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> grid partitoin unit tests
!------------------------------------------------------------------------------
module grid_test
  use shr_grid_mod, only: shr_grid
  use shr_gridcell_mod, only: GRIDCELL_N_NEAST, GRIDCELL_N_NORTH, GRIDCELL_N_EAST, &
            GRIDCELL_N_SEAST, GRIDCELL_N_SOUTH, GRIDCELL_N_SWEST, GRIDCELL_N_WEST, &
            GRIDCELL_N_NWEST, shr_gridcell
  use shr_coord_mod, only: coord
  use shr_testSuite_mod, only: testSuite
  use shr_precision_mod, only: sp

  implicit none

  private
  public :: testSuiteGrid

  type, extends(testSuite) :: testSuiteGrid

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGrid), intent(inout) :: self
    type(shr_grid) :: g

    !
    ! land       coordinates (center)   global indices 
    !
    ! x x   ->      (2,-1), (2, 1)   ->   1, 2 
    ! x x   ->      (0,-1), (0, 1)   ->   3, 4
    !
    !
    real(kind=sp), parameter :: limits(4) = [3, -1, 2, -2] !< N, S, E, W
    real(kind=sp), parameter :: resolution = 2.
    integer, parameter :: curPartition = 0
    integer, parameter :: nPartitions = 1
    type(shr_gridcell) :: gcEast, gcSeast, gcSouth, gcNeigh, gcFirst
    type(coord) :: ccenter, ccEast
    type(shr_grid) :: subgrid
    real(kind=sp) :: sublimits(4)
    type(coord) :: coordOut, coordIn
    type(coord), allocatable :: allCoords(:)
    type(shr_gridcell), allocatable :: allGridcells(:)
    real(kind=sp) :: mapTemp(2,2)
    real(kind=sp), allocatable :: landTemp(:)
    integer, allocatable :: gloIndices(:)
    type(coord) :: filterCoords(2)
    real(kind=sp), allocatable :: filteredTemp(:)
    real(kind=sp) :: temp1d(3) 

    g = shr_grid(limits, resolution, curPartition, nPartitions)

    !procedure :: hasNeighbourByIdx !< true if the given direction has a neighbour
    call self % assert(.not. g % hasNeighbourByIdx(1, GRIDCELL_N_NORTH), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, north) == F" )
    call self % assert(.not. g % hasNeighbourByIdx(1, GRIDCELL_N_NEAST), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, north east) == F" )
    call self % assert( g % hasNeighbourByIdx(1, GRIDCELL_N_EAST), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, east) == T" )
    call self % assert(g % hasNeighbourByIdx(1, GRIDCELL_N_SEAST), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, south east) == T" )
    call self % assert(g % hasNeighbourByIdx(1, GRIDCELL_N_SOUTH), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, south) == T" )
    call self % assert(.not. g % hasNeighbourByIdx(1, GRIDCELL_N_SWEST), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, south west) == F" )
    call self % assert(.not. g % hasNeighbourByIdx(1, GRIDCELL_N_WEST), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, west) == F" )
    call self % assert(.not. g % hasNeighbourByIdx(1, GRIDCELL_N_NWEST), & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, north west) == F" )

    !procedure :: hasNeighbourByGc !< true if the given direction has a neighbour
    ccenter = coord(2, -1)
    gcFirst = shr_gridcell(1, 2., ccenter, .true.)
    call self % assert( g % hasNeighbourByGc(gcFirst, GRIDCELL_N_EAST), & 
            "grid(1, 2, 3, 4) % hasNeighbourByGc(1, east) == T" )

    call self % assert(.not. g % hasNeighbourByGc(gcFirst, GRIDCELL_N_NORTH), & 
            "grid(1, 2, 3, 4) % hasNeighbourByGc(1, north) == F" )

    !procedure :: getNeighbourByGc !< it returns the pointer of the neighbouring gridcell. Error if it does not exists.
    ccenter = coord(2, -1)
    gcFirst = shr_gridcell(1, 2., ccenter, .true.)

    ccEast = coord(2, 1)
    gcEast = shr_gridcell(2, 2., ccEast, .true.)
    call self % assert( g % getNeighbourByGc(gcFirst, GRIDCELL_N_EAST) == gcEast, & 
            "grid([3, -1, 2, -2]) % getNeighbourByGc(1, east) .eq. 2 == T" )
   

    !procedure :: getNeighbourByIdx !< it returns the pointer of the neighbouring gridcell. Error if it does not exists.
    ccenter = coord(2, 1)
    gcEast = shr_gridcell(2, 2., ccenter, .true.)
    gcNeigh = g % getNeighbourByIdx(1, GRIDCELL_N_EAST)
    call self % assert( g % getNeighbourByIdx(1, GRIDCELL_N_EAST) == gcEast, & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, east) .eq. 2 == T" )


    ccenter = coord(0, 1)
    gcSeast = shr_gridcell(4, 2., ccenter, .true.)
    gcNeigh = g % getNeighbourByIdx(1, GRIDCELL_N_SEAST)

    call self % assert(g % getNeighbourByIdx(1, GRIDCELL_N_SEAST) == gcSeast, & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, south east) .eq. gc(idx=) == T" )


    ccenter = coord(0, -1)
    gcSouth = shr_gridcell(3, 2., ccenter, .true.)
    gcNeigh = g % getNeighbourByIdx(1, GRIDCELL_N_SOUTH)

    call self % assert(g % getNeighbourByIdx(1, GRIDCELL_N_SOUTH) == gcSouth, & 
            "grid([3, -1, 2, -2]) % getNeighbourByIdx(1, south) .eq. south == T" )


    !procedure :: getSubGrid !< it returns a new grid with the given coordinates
    sublimits = [1, -1, 1, -1]
    subgrid = g % getSubgrid(sublimits)     
    call self % assert( all( subgrid % getLimits() == [1, -1, 1, -1] ), &
             "g % getSubgrid() % getLimits() = [1, -1, 1, -1] == T")

    !procedure :: fitsInWindow !< true if the given grid fits in the current one 
    coordOut = coord(5., 1.)
    call self % assert( .not. g % fitsInCoord(coordOut), "g % fitsInCoord(5, 1) == F" )

    !procedure :: fitsInCoord !< true if the given coordinate fits in the current one 
    coordOut = coord(5., 1.)
    coordIn = coord(2., 0.)
    call self % assert( .not. g % fitsInCoord(coordOut), &
              "g([3, -1, 2, -2]) % fitsInCoord(5, 1) == F" )
    call self % assert( g % fitsInCoord(coordIn), &
              "g([3, -1, 2, -2]) % fitsInCoord(2, 0) == T" )


    !procedure :: getLimits !< max/min for lat and lon
    call self % assert(all( g % getLimits() == [3, -1, 2, -2] ), &
            "grid([3, -1, 2, -2]) % getLimits() .eq. [3, -1, 2, -2] == T" )

    !procedure :: getResolution !< return resolution
    call self % assert(g % getResolution() == 2., &
            "grid(..., res=2) % getResolution() .eq. 2. == T" )

    !procedure :: getLatCoords !< get all latitude coordinates
    call self % assert(all(g % getLatCoords() == [2., 0.]), &
            "grid([3, -1, ...]) % getLatCoords() .eq. [2, 0] == T" )

    !procedure :: getLonCoords !< get all longitude coordinates
    call self % assert(all(g % getLonCoords() == [1., -1.]), &
            "grid([..., 2, -2]) % getLonCoords() .eq. [1, -1] == T" )

    !procedure :: latCoord2Index !< get all latitude indices from a lat coordinate
    call self % assert(all(g % latCoord2Index(2.5) == [1]), &
            "grid([3, -1, ...]) % latCoord2Index(2.5) .eq. [1] == T" )

    !procedure :: lonCoord2Index !< get all longitude indices from a long coordinate
    call self % assert(all(g % latCoord2Index(-1.5) == [2]), &
            "grid([3, -1, ...]) % latCoord2Index(2.5) .eq. [2] == T" )

    !procedure :: getLatSize !< get the total number of latitudes
    call self % assert(g % getLatSize() == 2, &
            "grid([3, -1, ...]) % getLatSize() .eq. 2 == T" )

    !procedure :: getLonSize !< get the total number of longitudes
    call self % assert(g % getLonSize() == 2, &
            "grid([..., 2, -2]) % getLonSize() .eq. 2 == T" )

    !procedure :: isGlobal !< true if has the whole world
    call self % assert(.not. g % isGlobal(), "grid([3, -1, 2, -2]) % isGlobal() == F" )

    !procedure :: countAllGridcells !< total number of gridcells
    call self % assert(g % countAllGridcells() == 4, &
              "grid([3, -1, 2, -2]) % countAllGridcells() .eq. 4 == T" )

    !procedure :: getGridcellBySpatialIndices !< Given lat and lon spatial array indices it returns a coordinate from
                                              !< the center of the gridcell
    ccenter = coord(2, 1)
    gcEast = shr_gridcell(2, 2., ccenter, .true.)
    call self % assert(g % getGridcellBySpatialIndices(1, 2) == gcEast, &
              "grid([3, -1, 2, -2]) % getGridcellBySpatialIndices(2.5, -1.5) .eq. gcEast == T" )

    !procedure :: getGridcellByCoord !< given a coordinate it finds the gridcell
    call self % assert(all (g % getGridcellByCoord(coord(2.5, 1.5)) == gcEast), &
              "grid([3, -1, 2, -2]) % getGridcellByCoord(2.5, 1.5) .eq. gcEast == T" )

    !procedure :: isCoordEnabled !< true if the given coordinate is enabled
    call self % assert( g % isCoordEnabled(coord(2.5, 1.5)), &
              "grid([3, -1, 2, -2]) % isCoordEnabled(2.5, 1.5) == T" )

    !procedure :: getAllEnabledGridcellsByCoords !< it returns the coordinates from the enabled gridcells
    allCoords = g % getAllEnabledGridcellsByCoords()
    call self % assert( size(allCoords) == 4, &
              "grid([3, -1, 2, -2]) % getAllEnabledGridcellsByCoords() .eq. 4 == T" )

    !procedure :: countAllEnabledGridcells !< it returns the number of enabled gridcells
    call self % assert( g % countAllEnabledGridcells() == 4, &
              "grid([3, -1, 2, -2]) % countAllEnabledGridcells() .eq. 4 == T" )

    !procedure :: setAllEnabledGridcells !< it defines a new list of enable gridcells
    call g % setAllEnabledGridcells([coord(2.5, 1.5)]) !< idx = 2
    call self % assert( g % countAllEnabledGridcells() == 1, &
              "grid([3, -1, 2, -2]) % setAllEnabledGridcells((2.5, 1.5)) .eq. 1 == T" )

    !procedure :: getAllEnabledGridcells !< it returns all enabled gridcells
    allgridcells = g % getAllEnabledGridcells()
    ccenter = coord(2, 1)
    gcEast = shr_gridcell(2, 2., ccenter, .true.)
    call self % assert( all( allgridcells == gcEast), &
            "grid([3, -1, 2, -2]) % getAllEnabledGridcells() .eq. gcEast == T" )

    !procedure :: setGridcellStatusByCoord !< it defines a new status for the given gridcell
                                          !< described as coordinate
    call g % setGridcellStatusByCoord(coord(2.5, -1.5), .true.) !< idx = 1
    allgridcells = g % getAllEnabledGridcells()
    call self % assert( size( allgridcells ) == 2, &
            "grid([3, -1, 2, -2]) % getAllEnabledGridcells() .eq. 2 == T" )

    !procedure :: setGridcellStatusByIdxs 
    call g % setGridcellStatusByIdxs(1, 1, .false.) !< lat index = 1, lon index = 1
    allgridcells = g % getAllEnabledGridcells()
    call self % assert( size( allgridcells ) == 1, &
            "grid([3, -1, 2, -2]) % getAllEnabledGridcells() .eq. 1 == T" )

    !procedure :: getGloIndices
    gloIndices = g % getGloIndices()
    call self % assert( all( gloIndices == [1, 2, 3, 4] ), &
            "grid([3, -1, 2, -2]) % getGloIndices() .eq. 1, 2, 3, 4 == T" )

    !< transformDataSpatialToEnabled_
    call g % setGridcellStatusByIdxs(2, 1, .true.) !< lat index = 2, lon index = 1
    call g % setGridcellStatusByIdxs(1, 1, .true.) !< lat index = 2, lon index = 1
    mapTemp(1,:) = [25., 26.]  ! T, T
    mapTemp(2,:) = [27., 28.]  ! T, F
    landTemp = g % transformDataSpatialToEnabled(mapTemp)
!    write(*,*) "grid_test:: landTemp = ", landTemp
    call self % assert( all( landTemp == [25., 27., 26.] ), &
            "grid % transformDataSpatialToEnabled([25, 26, 27, 28]) .eq. [25, 26, 27] == T" )

    !< filterEnabledData_  
    temp1d = [25., 26., 27.]
    filterCoords(1) = coord(2., -1.) !< idx = 1
    filterCoords(2) = coord(0., -1.) !< idx = 3
    filteredTemp = g % filterEnabledData(temp1d, filterCoords)
!    write(*,*) "grid_test:: filterEnabledData =", filteredTemp
    call self % assert( all( filteredTemp == [25., 27.] ), &
            "grid % filterEnabledData([25, 26, 27]) .eq. [25, 27] == T" )

  end subroutine defineTestCases

end module grid_test

