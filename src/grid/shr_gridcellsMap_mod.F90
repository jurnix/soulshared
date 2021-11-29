!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcellsMap_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> shr_gridcellsMap computes coordinates into array indices
!> 
!------------------------------------------------------------------------------
module shr_gridcellsMap_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_coord_mod, only: shr_coord
  use shr_gridcell_mod, only: shr_gridcell
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gAxisMapping_mod, only: shr_gAxisMapping


  implicit none

  public :: shr_gridcellsMap

  logical, parameter :: ISDEBUG = .false.

  type shr_gridcellsMapIterator
  contains
!    procedure :: hasMore -> yes/no
!    procedure :: getNext -> shr_gridcellsMap
  end type shr_gridcellsMapIterator


  type shr_gridcellsMap
    type(shr_gridcell), allocatable :: gridcells(:,:)

    type(shr_gAxisMapping), allocatable :: latMapping
    type(shr_gAxisMapping), allocatable :: lonMapping

!    real(kind=sp) :: resolution
!    type(shr_gGridAxes), allocatable :: latitudes
!    type(shr_gGridAxes), allocatable :: longitudes
  contains
    procedure :: init => gridcellsMap_initialize 
    ! procedure :: createGridcellsMapIterator() -> shr_gridcellsMapIterator

    procedure :: getIndexByCoord
    generic :: getIndex => getIndexByCoord

!    procedure :: getIndexByGridcell
!    generic :: getIndex => getIndexByGridcell
  end type shr_gridcellsMap

contains

  subroutine gridcellsMap_initialize(self, resolution, latAxis, lonAxis)
    !< gridcellsMap initialization
    !<
    class(shr_gridcellsMap), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gGridAxes), intent(in) :: latAxis
    type(shr_gGridAxes), intent(in) :: lonAxis

!    self % resolution = resolution
!    allocate(self % latitudes, source = latAxes)
!    allocate(self % longitudes, source = lonAxes)

    ! gridcells mapping indices
    allocate(self % latMapping)
    call self % latMapping % init(latAxis)
    allocate(self % lonMapping)
    call self % lonMapping % init(lonAxis)

    ! populate gridcells
!    self % gridcells = self % latitudes % expand(self % longitudes)
  end subroutine gridcellsMap_initialize


  function getIndexByCoord(self, coord) result (gIndices)
    !< given a coordinate it returns the matching indices 
    !< From 1 to 4 possible  
    !< coord must be inside the grid bounds
    use shr_gAxisMapping_mod, only: shr_gAxisMapping
    class(shr_gridcellsMap), intent(in) :: self
    type(shr_coord), intent(in) :: coord
    type(shr_gridcellIndex), allocatable :: gindices(:)

    integer, allocatable :: idxlats(:), idxlons(:) 
    integer :: ilat, ilon
    integer :: nlats, nlons
    integer :: icell

    !< calculate indices 
    idxlats = self % latMapping % getIndex(coord % lat)
    idxlons = self % latMapping % getIndex(coord % lon)
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
  end function getIndexByCoord


!  type(shr_gridcellIndex) function getIndexByGridcell(self, gc)
    !< it returns a gridcell index by providing a 'gridcell'
!    class(shr_gridcellsMap), intent(in) :: self
!    type(shr_gridcell), intent(in) :: gc
!  end function getIndexByGridcell


end module shr_gridcellsMap_mod 

