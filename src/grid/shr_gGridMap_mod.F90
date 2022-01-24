!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridMap_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridMap
!>
!------------------------------------------------------------------------------
module shr_gGridMap_mod

  use shr_gGridDescriptor_mod, only: shr_igGridDescriptor
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_coord_mod, only: shr_coord
  use shr_gridShape_mod, only: shr_gridShape

  use shr_strings_mod, only: string
  use shr_precision_mod, only: sp
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private

  public :: shr_gGridMap, shr_gridMapBuilder

  character(*), parameter :: LATITUDE_NAME = "latitude"
  character(*), parameter :: LONGITUDE_NAME = "longitude"

  integer, parameter :: SHR_GRIDMAP_POSITION_FIRST = 1
  integer, parameter :: SHR_GRIDMAP_POSITION_LAST = 2
  !< to add, top-left, top..., bottom...

  !< implementation
  type :: shr_gGridMap
    class(shr_igGridDescriptor), allocatable :: gridDescriptor
    !< latitude axis mapping
    class(shr_gAxisMapping), allocatable :: latAxMapping, lonAxMapping
  contains
    procedure :: init
    procedure :: equal
    generic :: operator(==) => equal

    !< getters
    procedure :: getGridDescriptor
    procedure :: getLatAxis
    procedure :: getLonAxis

    procedure :: getIndex
    procedure :: getShape
  end type shr_gGridMap

contains


  subroutine init(self, gridDescriptor, latAxMapping, lonAxMapping)
    !< initialize
    class(shr_gGridMap), intent(inout) :: self
    class(shr_igGridDescriptor), intent(in) :: gridDescriptor
    type(shr_gAxisMapping), intent(in) :: latAxMapping, lonAxMapping
    allocate(self % gridDescriptor, source = gridDescriptor)
    allocate(self % latAxMapping, source = latAxMapping)
    allocate(self % lonAxMapping, source = lonAxMapping)
  end subroutine init


  type(shr_gAxisMapping) function getLatAxis(self)
    !< latAxis getter
    class(shr_gGridMap), intent(in) :: self
    getLatAxis = self % latAxMapping
  end function getLatAxis


  type(shr_gAxisMapping) function getLonAxis(self)
    !< lonAxis getter
    class(shr_gGridMap), intent(in) :: self
    getLonAxis = self % lonAxMapping
  end function getLonAxis


  function getGridDescriptor(self) result (gDesc)
    !< getGridDescriptor getter
    class(shr_gGridMap), intent(in) :: self
    class(shr_igGridDescriptor), allocatable :: gDesc
    allocate(gDesc, source = self % gridDescriptor)
  end function getGridDescriptor


  function getIndex(self, coord) result (gIndices)
    !< Seach gridcell indices from given 'coord'
    !< Multiple values can be returned if given coord
    !< is found in-between multiple gridcells
    class(shr_gGridMap), intent(in) :: self
    type(shr_coord), intent(in) :: coord
    type(shr_gridcellIndex), allocatable :: gIndices(:)!< output

    integer, allocatable :: idxlats(:), idxlons(:)
    integer :: ilat, ilon
    integer :: nlats, nlons
    integer :: icell

    !< calculate indices
    idxlats = self % latAxMapping % getIndex(coord % lat)
    idxlons = self % lonAxMapping % getIndex(coord % lon)
    nlats = size(idxlats)
    nlons = size(idxlons)

    allocate(gIndices(nlats * nlons))

    ! populate output
    icell = 1
    do ilat = 1, nlats
      do ilon = 1, nlons
        call gIndices(icell) % init(idxlats(ilat), idxlons(ilon))
        icell = icell + 1 ! next
      enddo ! nlons
    enddo ! nlats
  end function getIndex


  type(shr_gridShape) function getShape(self)
    !< grid shape
    class(shr_gGridMap), intent(in) :: self
    getShape % nlats = self % latAxMapping % getSize()
    getShape % nlons = self % lonAxMapping % getSize()
  end function getShape


  function shr_gridMapBuilder(gDescriptor) result (newGridMap)
    !< creates a new gridmap from 'gDescriptor'
    class(shr_igGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridMap) :: newGridMap

    type(shr_gridBounds) :: bounds
    real(kind=sp) :: resolution
    type(shr_gGridAxesBounds) :: laxisBounds, lonxisBounds
    type(shr_gGridAxes) :: laxis, lonxis
    type(shr_gAxisMapping) :: laxisMapping, lonxisMapping

    resolution = gDescriptor % getResolution()
    bounds = gDescriptor % getBounds()

    call laxisBounds % init(bounds % getNorth(), bounds % getSouth())
    call lonxisBounds % init(bounds % getEast(), bounds % getWest())

    call laxis % init(string(LATITUDE_NAME), resolution, laxisBounds)
    call lonxis % init(string(LONGITUDE_NAME), resolution, lonxisBounds)

    call laxisMapping % init(laxis)
    call lonxisMapping % init(lonxis)

    call newGridMap % init(gDescriptor, laxisMapping, lonxisMapping)
  end function shr_gridMapBuilder


  logical function equal(self, other)
    !< true if self and other have the same attributes
    class(shr_gGridMap), intent(in) :: self
    class(shr_gGridMap), intent(in) :: other
    logical :: hasSameGridDescriptor
    logical :: hasSameLaxisMapping, hasSameLonxisMapping

    hasSameGridDescriptor = (self % gridDescriptor == other % getGridDescriptor())
    hasSameLaxisMapping = (self % latAxMapping == other % getLatAxis())
    hasSameLonxisMapping = (self % lonAxMapping == other % getLonAxis())

    equal = (hasSameGridDescriptor .and. &
          hasSameLaxisMapping .and. hasSameLonxisMapping)
  end function equal


end module shr_gGridMap_mod
