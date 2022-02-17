!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridMapCoords_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Coordinates mapping from grid
!> - Provides coordinates for any type of input
!>
!------------------------------------------------------------------------------
module shr_gGridMapCoords_mod

  use shr_precision_mod, only: sp
  use shr_strings_mod, only: string

  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
  use shr_coord_mod, only: shr_coord
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gridBoundIndices_mod, only: shr_gridBoundIndices
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gAxis_mod, only: shr_igAxis, shr_gAxis
  use shr_gAxisCell_mod, only: shr_gAxisCell
  use shr_gridcell_mod, only: shr_gridcell
  use shr_gAxisBounds_mod, only: shr_gAxisBounds

  use shr_gGridArrayMap_mod, only: LATITUDE_NAME, LONGITUDE_NAME


  implicit none

  private

  public :: shr_gGridMapCoords, shr_gGridMapCoordsBuilder


  !<
  type :: shr_gGridMapCoords
    class(shr_iGGridDescriptor), allocatable :: gridDescriptor
    class(shr_igAxis), allocatable :: laxis
    class(shr_igAxis), allocatable :: lonxis
  contains
    procedure :: init

    procedure, private :: getCoord_byGridIndex
    procedure, private :: getCoord_byGridBoundIndices
    generic :: getCoord => getCoord_byGridIndex, getCoord_byGridBoundIndices

    procedure :: getGridcell
  end type shr_gGridMapCoords

contains

  subroutine init(self, gridDescriptor, laxis, lonxis)
    !< initialization
    class(shr_gGridMapCoords), intent(inout) :: self
    class(shr_iGGridDescriptor), intent(in) :: gridDescriptor
    class(shr_igAxis), intent(in) :: laxis, lonxis
    allocate(self % gridDescriptor, source = gridDescriptor)
    allocate(self % laxis, source = laxis)
    allocate(self % lonxis, source = lonxis)
  end subroutine init


  function getGridcell(self, gcIndex) result (gc)
    !< Given a gridcellIndex it returns the gridcell
    !< gcIndex indices must fit in the arrays
    class(shr_gGridMapCoords), intent(in) :: self
    type(shr_gridcellIndex), intent(in) :: gcIndex
    type(shr_gridcell) :: gc

    type(shr_gAxisCell) :: latcell, loncell

    !< find gAxisCell
    latCell = self % laxis % getCell(gcIndex % idxlat)
    lonCell = self % lonxis % getCell(gcIndex % idxlon)

    !< create gridcell
    gc = latCell * lonCell
  end function getGridcell


  function getCoord_byGridIndex(Self, gcIndex) result (coord)
    !< Given a gridcell index it returns a coordinate
    !< By default the coordinate repesents its center
    !< gcIndex must fit into self % grid, otherwise an error is raised
    class(shr_gGridMapCoords), intent(in) :: self
    type(shr_gridcellIndex), intent(in) :: gcIndex
    type(shr_coord) :: coord !< output
    type(shr_gridcell) :: gc

    gc = self % getGridcell(gcIndex)
    coord = gc % getCenter()
  end function getCoord_byGridIndex


  function getCoord_byGridBoundIndices(self, gbIndices) result (gridBounds)
    !< Grive grid bound indices it returns its grid bounds coordinates
    !< gbIndices must fit into self % grid, otherwise an error is raised
    class(shr_gGridMapCoords), intent(in) :: self
    type(shr_gridBoundIndices), intent(in) :: gbIndices
    type(shr_gridBounds) :: gridBounds !< output

    type(shr_gridcellIndex) :: northWestGCI, southEastGCI
    type(shr_gridcell) :: seGC, nwGC
    real(kind=sp) :: north, south, east, west

    !< north index
    northWestGCI % idxlat = gbIndices % getStartRow()
    !< west index
    northWestGCI % idxlon = gbIndices % getStartCol()

    !< south index
    southEastGCI % idxlat = gbIndices % getEndRow()
    !< east index
    southEastGCI % idxlon = gbIndices % getEndCol()

    !< into coordinates
    seGC = self % getGridcell(southEastGCI)
    south = seGC % getSouth()
    east = seGC % getEast()
    nwGC = self % getGridcell(northWestGCI)
    north = nwGC % getNorth()
    west = nwGC % getWest()

    call gridBounds % init(north, south, east, west)
  end function getCoord_byGridBoundIndices


  function shr_gGridMapCoordsBuilder(gDescriptor) result (newGridMapCoords)
    !< creates a new gridcells map from 'gDescriptor'
    class(shr_igGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridMapCoords) :: newGridMapCoords

    type(shr_gridBounds) :: bounds
    real(kind=sp) :: resolution
    type(shr_gAxisBounds) :: laxisBounds, lonxisBounds
    type(shr_gAxis) :: laxis, lonxis

    resolution = gDescriptor % getResolution()
    bounds = gDescriptor % getBounds()

    call laxisBounds % init(bounds % getNorth(), bounds % getSouth())
    call lonxisBounds % init(bounds % getEast(), bounds % getWest())

    call laxis % init(string(LATITUDE_NAME), resolution, laxisBounds)
    call lonxis % init(string(LONGITUDE_NAME), resolution, lonxisBounds)

    call newGridMapCoords % init(gDescriptor, laxis, lonxis)
  end function shr_gGridMapCoordsBuilder


end module shr_gGridMapCoords_mod
