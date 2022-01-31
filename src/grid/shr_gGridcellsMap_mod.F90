!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridCellsMap_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridcells mapping to handle coordinates vs gridcells
!>
!------------------------------------------------------------------------------
module shr_gGridCellsMap_mod

  use shr_gGridDescriptor_mod, only: shr_igGridDescriptor
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_coord_mod, only: shr_coord
  use shr_gridShape_mod, only: shr_gridShape
  use shr_gridcell_mod, only: shr_gridcell

  use shr_strings_mod, only: string
  use shr_precision_mod, only: sp
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gAxis_mod, only: shr_gAxis, shr_igAxis
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gAxisCell_mod, only: shr_gAxisCell

  use shr_gGridArrayMap_mod, only: LATITUDE_NAME, LONGITUDE_NAME

  implicit none

  private

  public :: shr_gGridCellsMap, shr_gGridCellsMapBuilder
  !public :: LATITUDE_NAME, LONGITUDE_NAME

  !character(*), parameter :: LATITUDE_NAME = "latitude"
  !character(*), parameter :: LONGITUDE_NAME = "longitude"

  !integer, parameter :: SHR_GRIDARRAYMAP_POSITION_FIRST = 1
  !integer, parameter :: SHR_GRIDARRAYMAP_POSITION_LAST = 2
  !< to add, top-left, top..., bottom...


  !< implementation
  type :: shr_gGridCellsMap
    class(shr_igGridDescriptor), allocatable :: gridDescriptor
    !< grid axis
    class(shr_igAxis), allocatable :: latax, lonax
  contains
    procedure :: init
    procedure :: equal => gGridCellsMap_equal
    generic :: operator(==) => equal

    !< getters
    procedure :: getGridDescriptor => gGridCellsMap_getGridDescriptor
    procedure :: getLatAxis => gGridCellsMap_getLatAxis
    procedure :: getLonAxis => gGridCellsMap_getLonAxis

    procedure :: getGridcells => gGridCellsMap_getGridcells
    procedure :: getShape => gGridCellsMap_getShape

    !<
    procedure :: toString => gGridCellsMap_toString
  end type shr_gGridCellsMap

contains


  subroutine init(self, gridDescriptor, latax, lonax)
    !< initialize
    class(shr_gGridCellsMap), intent(inout) :: self
    class(shr_igGridDescriptor), intent(in) :: gridDescriptor
    class(shr_igAxis), intent(in) :: latAx, lonAx
    allocate(self % gridDescriptor, source = gridDescriptor)
    allocate(self % latAx, source = latAx)
    allocate(self % lonAx, source = lonAx)
  end subroutine init


  function gGridCellsMap_getLatAxis(self) result (laxis)
    !< latAxis getter
    class(shr_gGridCellsMap), intent(in) :: self
    class(shr_igAxis), allocatable :: laxis !< output
    allocate(laxis, source = self % latAx)
  end function gGridCellsMap_getLatAxis


  function gGridCellsMap_getLonAxis(self) result (lonxis)
    !< lonAxis getter
    class(shr_gGridCellsMap), intent(in) :: self
    class(shr_igAxis), allocatable :: lonxis
    allocate(lonxis, source = self % lonax)
  end function gGridCellsMap_getLonAxis


  function gGridCellsMap_getGridDescriptor(self) result (gDesc)
    !< getGridDescriptor getter
    class(shr_gGridCellsMap), intent(in) :: self
    class(shr_igGridDescriptor), allocatable :: gDesc
    allocate(gDesc, source = self % gridDescriptor)
  end function gGridCellsMap_getGridDescriptor


  function gGridCellsMap_getGridcells(self, coord) result (gcs)
    !< Seach gridcell indices from given 'coord'
    !< Multiple values can be returned if given coord
    !< is found in-between multiple gridcells
    class(shr_gGridCellsMap), intent(in) :: self
    type(shr_coord), intent(in) :: coord
    type(shr_gridcell), allocatable :: gcs(:)!< output

    type(shr_gAxisCell), allocatable :: latCells(:), lonCells(:)
    integer, allocatable :: idxlats(:), idxlons(:)
    integer :: ilat, ilon
    integer :: nlats, nlons
    integer :: icell
    type(shr_gridcell), allocatable :: tmpgcs(:)!< output

    !< calculate indices
    latCells = self % latax % getCells(coord % lat)
    lonCells = self % lonax % getCells(coord % lon)
    nlats = size(latCells)
    nlons = size(lonCells)

    allocate(gcs(0)) !< empty
    do ilat = 1, size(latCells)
      tmpgcs = latCells(ilat) * lonCells
      gcs = [gcs, tmpgcs]
    end do
    !write(*,*) "shr_gGridcellsMap_mod:: gGridCellsMap_getGridcells:: gcs size?", size(gcs)

  end function gGridCellsMap_getGridcells


  type(shr_gridShape) function gGridCellsMap_getShape(self)
    !< grid shape
    class(shr_gGridCellsMap), intent(in) :: self
    gGridCellsMap_getShape % nlats = self % latAx % getSize()
    gGridCellsMap_getShape % nlons = self % lonAx % getSize()
  end function gGridCellsMap_getShape

!
!  function shr_gridArrayMapBuilder(gDescriptor) result (newGridMap)
!    !< creates a new gridmap from 'gDescriptor'
!    class(shr_igGridDescriptor), intent(in) :: gDescriptor
!    type(shr_gGridCellsMap) :: newGridMap
!
!    type(shr_gridBounds) :: bounds
!    real(kind=sp) :: resolution
!    type(shr_gGridAxesBounds) :: laxisBounds, lonxisBounds
!    type(shr_gGridAxes) :: laxis, lonxis
!    type(shr_gAxisMapping) :: laxisMapping, lonxisMapping
!
!    resolution = gDescriptor % getResolution()
!    bounds = gDescriptor % getBounds()
!
!    call laxisBounds % init(bounds % getNorth(), bounds % getSouth())
!    call lonxisBounds % init(bounds % getEast(), bounds % getWest())

  !  call laxis % init(string(LATITUDE_NAME), resolution, laxisBounds)
  !  call lonxis % init(string(LONGITUDE_NAME), resolution, lonxisBounds)

  !  call laxisMapping % init(laxis)
  !  call lonxisMapping % init(lonxis)

  !  call newGridMap % init(gDescriptor, laxisMapping, lonxisMapping)
  !end function shr_gridArrayMapBuilder


  logical function gGridCellsMap_equal(self, other)
    !< true if self and other have the same attributes
    class(shr_gGridCellsMap), intent(in) :: self
    class(shr_gGridCellsMap), intent(in) :: other
    logical :: hasSameGridDescriptor
    logical :: hasSameLaxis, hasSameLonxis

    class(shr_igAxis), allocatable :: tmpAxis

    if (.not. same_type_as(self, other)) then
      !< unexpected type found
      gGridCellsMap_equal = .false.
      return
    end if

    hasSameGridDescriptor = (self % gridDescriptor == other % getGridDescriptor())
    tmpAxis = other % getLatAxis()
    hasSameLaxis = (self % latax == tmpAxis)
    tmpAxis = other % getLonAxis()
    hasSameLonxis = (self % lonax == tmpAxis)

    !write(*,*) "shr_gGridCellsMap:: equal:: hasSameGridDescriptor= ", hasSameGridDescriptor
    !write(*,*) "shr_gGridCellsMap:: equal:: hasSameLaxisMapping= ", hasSameLaxisMapping
    !write(*,*) "shr_gGridCellsMap:: equal:: hasSameLonxisMapping= ", hasSameLonxisMapping

    gGridCellsMap_equal = (hasSameGridDescriptor .and. &
          hasSameLaxis .and. hasSameLonxis)
  end function gGridCellsMap_equal


  type(string) function gGridCellsMap_toString(self)
    !< string representation of shr_gGridCellsMap
    class(shr_gGridCellsMap), intent(in) :: self
    gGridCellsMap_toString = self % gridDescriptor % toString() + ", lat=(" +&
               self % latax % toString() + ") - lon=(" + &
               self % lonax% toSTring() + ")"
  end function gGridCellsMap_toString


  function shr_gGridCellsMapBuilder(gDescriptor) result (newGridcellsMap)
    !< creates a new gridcells map from 'gDescriptor'
    class(shr_igGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridcellsMap) :: newGridcellsMap

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

    call newGridcellsMap % init(gDescriptor, laxis, lonxis)
  end function shr_gGridCellsMapBuilder

end module shr_gGridCellsMap_mod
