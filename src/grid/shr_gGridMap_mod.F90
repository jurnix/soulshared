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
!> gridMap mapping vs 2d array to handle coordinates vs array indices
!>
!------------------------------------------------------------------------------
module shr_gGridMap_mod

  use shr_gGridDescriptor_mod, only: shr_igGridDescriptor
  use shr_gAxisMapping_mod, only: shr_gAxisMapping, shr_igAxisMapping
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_coord_mod, only: shr_coord
  use shr_gridShape_mod, only: shr_gridShape
  use shr_gridcell_mod, only: shr_gridcell

  use shr_strings_mod, only: string
  use shr_precision_mod, only: sp
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private

  public :: shr_gGridMap, shr_gridMapBuilder, shr_igGridMap
  public :: LATITUDE_NAME, LONGITUDE_NAME

  character(*), parameter :: LATITUDE_NAME = "latitude"
  character(*), parameter :: LONGITUDE_NAME = "longitude"

  integer, parameter :: SHR_GRIDMAP_POSITION_FIRST = 1
  integer, parameter :: SHR_GRIDMAP_POSITION_LAST = 2
  !< to add, top-left, top..., bottom...


  type, abstract :: shr_igGridMap
  contains
    procedure(iface_getAxis), deferred :: getLatAxis
    procedure(iface_getAxis), deferred :: getLonAxis
    procedure(iface_getGridDescriptor), deferred :: getGridDescriptor
    procedure(iface_getShape), deferred :: getShape
    procedure(iface_getIndex), deferred :: getIndex
    procedure(iface_toString), deferred :: toString
    procedure(iface_equal), deferred :: equal
    generic :: operator(==) => equal
  end type shr_igGridMap


  abstract interface
    type(shr_gridShape) function iface_getShape(self)
      import :: shr_igGridMap, shr_gridShape
      !< grid shape
      class(shr_igGridMap), intent(in) :: self
    end function iface_getShape

    function iface_getIndex(self, coord) result (gIndices)
      import :: shr_igGridMap, shr_coord, shr_gridcellIndex
      !< Seach gridcell indices from given 'coord'
      !< Multiple values can be returned if given coord
      !< is found in-between multiple gridcells
      class(shr_igGridMap), intent(in) :: self
      type(shr_coord), intent(in) :: coord
      type(shr_gridcellIndex), allocatable :: gIndices(:)!< output
    end function iface_getIndex

    type(string) function iface_toString(self)
      import :: shr_igGridMap, string
      !< string representation of shr_gGridMap
      class(shr_igGridMap), intent(in) :: self
    end function iface_toString

    logical function iface_equal(self, other)
      import :: shr_igGridMap
      !< true if self and other have the same attributes
      class(shr_igGridMap), intent(in) :: self
      class(shr_igGridMap), intent(in) :: other
    end function iface_equal

    function iface_getGridDescriptor(self) result (gDesc)
      import :: shr_igGridMap, shr_igGridDescriptor
      !< getGridDescriptor getter
      class(shr_igGridMap), intent(in) :: self
      class(shr_igGridDescriptor), allocatable :: gDesc
    end function iface_getGridDescriptor

    function iface_getAxis(self) result (laxisMapping)
      import :: shr_igGridMap, shr_igAxisMapping
      !< latAxis getter
      class(shr_igGridMap), intent(in) :: self
      class(shr_igAxisMapping), allocatable :: laxisMapping !< output
    end function iface_getAxis
  end interface


  !< implementation
  type, extends(shr_igGridMap) :: shr_gGridMap
    class(shr_igGridDescriptor), allocatable :: gridDescriptor
    !< latitude axis mapping
    class(shr_igAxisMapping), allocatable :: latAxMapping, lonAxMapping
  contains
    procedure :: init
    procedure :: equal => gGridMap_equal
    !generic :: operator(==) => equal

    !< getters
    procedure :: getGridDescriptor => gGridMap_getGridDescriptor
    procedure :: getLatAxis => gGridMap_getLatAxis
    procedure :: getLonAxis => gGridMap_getLonAxis

    procedure :: getIndex => gGridMap_getIndex
    procedure :: getShape => gGridMap_getShape

    !<
    procedure :: toString => gGridMap_toString
  end type shr_gGridMap

contains


  subroutine init(self, gridDescriptor, latAxMapping, lonAxMapping)
    !< initialize
    class(shr_gGridMap), intent(inout) :: self
    class(shr_igGridDescriptor), intent(in) :: gridDescriptor
    class(shr_igAxisMapping), intent(in) :: latAxMapping, lonAxMapping
    allocate(self % gridDescriptor, source = gridDescriptor)
    allocate(self % latAxMapping, source = latAxMapping)
    allocate(self % lonAxMapping, source = lonAxMapping)
  end subroutine init


  function gGridMap_getLatAxis(self) result (laxisMapping)
    !< latAxis getter
    class(shr_gGridMap), intent(in) :: self
    class(shr_igAxisMapping), allocatable :: laxisMapping !< output
    allocate(laxisMapping, source = self % latAxMapping)
  end function gGridMap_getLatAxis


  function gGridMap_getLonAxis(self) result (lonxisMapping)
    !< lonAxis getter
    class(shr_gGridMap), intent(in) :: self
    class(shr_igAxisMapping), allocatable :: lonxisMapping
    allocate(lonxisMapping, source = self % lonAxMapping)
  end function gGridMap_getLonAxis


  function gGridMap_getGridDescriptor(self) result (gDesc)
    !< getGridDescriptor getter
    class(shr_gGridMap), intent(in) :: self
    class(shr_igGridDescriptor), allocatable :: gDesc
    allocate(gDesc, source = self % gridDescriptor)
  end function gGridMap_getGridDescriptor


  function gGridMap_getIndex(self, coord) result (gIndices)
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
  end function gGridMap_getIndex


  type(shr_gridShape) function gGridMap_getShape(self)
    !< grid shape
    class(shr_gGridMap), intent(in) :: self
    gGridMap_getShape % nlats = self % latAxMapping % getSize()
    gGridMap_getShape % nlons = self % lonAxMapping % getSize()
  end function gGridMap_getShape


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


  logical function gGridMap_equal(self, other)
    !< true if self and other have the same attributes
    class(shr_gGridMap), intent(in) :: self
    class(shr_igGridMap), intent(in) :: other
    logical :: hasSameGridDescriptor
    logical :: hasSameLaxisMapping, hasSameLonxisMapping

    class(shr_gAxisMapping), allocatable :: tmpAxis

    if (.not. same_type_as(self, other)) then
      !< unexpected type found
      gGridMap_equal = .false.
      return
    end if

    hasSameGridDescriptor = (self % gridDescriptor == other % getGridDescriptor())
    tmpAxis = other % getLatAxis()
    hasSameLaxisMapping = (self % latAxMapping == tmpAxis)
    tmpAxis = other % getLonAxis()
    hasSameLonxisMapping = (self % lonAxMapping == tmpAxis)

    !write(*,*) "shr_gGridMap:: equal:: hasSameGridDescriptor= ", hasSameGridDescriptor
    !write(*,*) "shr_gGridMap:: equal:: hasSameLaxisMapping= ", hasSameLaxisMapping
    !write(*,*) "shr_gGridMap:: equal:: hasSameLonxisMapping= ", hasSameLonxisMapping

    gGridMap_equal = (hasSameGridDescriptor .and. &
          hasSameLaxisMapping .and. hasSameLonxisMapping)
  end function gGridMap_equal


  type(string) function gGridMap_toString(self)
    !< string representation of shr_gGridMap
    class(shr_gGridMap), intent(in) :: self
    gGridMap_toString = self % gridDescriptor % toString() + ", lat=(" +&
               self % latAxMapping % toString() + ") - lon=(" + &
               self % lonAxMapping% toSTring() + ")"
  end function gGridMap_toString

end module shr_gGridMap_mod
