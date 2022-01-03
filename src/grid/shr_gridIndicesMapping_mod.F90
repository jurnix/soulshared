!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridIndicesMapping_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> shr_gridIndicesMapping computes coordinates into array indices
!>
!>
!>
!>      -180     0     180
!>        
!>    90  +------+------+
!>        |      |      |
!>        |  1,1 |  1,2 |
!>     0 -+------+------+
!>        |      |      |
!>        |  2,1 |  2,2 |
!>   -90 -+------+------+
!>
!> array(2,2)
!> 
!------------------------------------------------------------------------------
module shr_gridIndicesMapping_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_coord_mod, only: shr_coord
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor


  implicit none

  public :: shr_gridIndicesMapping

  logical, parameter :: ISDEBUG = .false.


  type shr_gridIndicesMapping
    class(shr_iGGridDescriptor), allocatable :: gDescriptor
    type(shr_gAxisMapping), allocatable :: latMapping
    type(shr_gAxisMapping), allocatable :: lonMapping
  contains
    !procedure :: gridIndicesMapping_initialize
    procedure :: gridIndicesMapping_initialize_byGridDescriptor
    generic :: init => gridIndicesMapping_initialize_byGridDescriptor!, gridIndicesMapping_initialize

    procedure :: getIndexByCoord
    generic :: getIndex => getIndexByCoord
  end type shr_gridIndicesMapping

contains


  subroutine gridIndicesMapping_initialize_byGridDescriptor(self, gDescriptor)
    !< gridIndicesMapping initialization
    !<
    class(shr_gridIndicesMapping), intent(inout) :: self
    class(shr_iGGridDescriptor), intent(in) :: gDescriptor
    type(shr_gGridAxes) :: latAxis, lonAxis

    allocate(self % gDescriptor, source=gDescriptor)
    latAxis = gDescriptor % getLatAxis()
    lonAxis = gDescriptor % getLonAxis()

    ! gridcells mapping indices
    allocate(self % latMapping)
    call self % latMapping % init(latAxis)
    allocate(self % lonMapping)
    call self % lonMapping % init(lonAxis)
  end subroutine gridIndicesMapping_initialize_byGridDescriptor


  !subroutine gridIndicesMapping_initialize(self, latAxis, lonAxis)
    !< gridIndicesMapping initialization
    !< todo: remove
!    class(shr_gridIndicesMapping), intent(inout) :: self
!    type(shr_gGridAxes), intent(in) :: latAxis
!    type(shr_gGridAxes), intent(in) :: lonAxis

    ! gridcells mapping indices
!    allocate(self % latMapping)
!    call self % latMapping % init(latAxis)
!    allocate(self % lonMapping)
!    call self % lonMapping % init(lonAxis)
!  end subroutine gridIndicesMapping_initialize


  function getIndexByCoord(self, coord) result (gIndices)
    !< given a coordinate it returns the matching indices 
    !< From 1 to 4 possible  
    !< coord must be inside the grid bounds
    class(shr_gridIndicesMapping), intent(in) :: self
    type(shr_coord), intent(in) :: coord
    type(shr_gridcellIndex), allocatable :: gindices(:)

    integer, allocatable :: idxlats(:), idxlons(:) 
    integer :: ilat, ilon
    integer :: nlats, nlons
    integer :: icell

    !< calculate indices 
    idxlats = self % latMapping % getIndex(coord % lat)
    idxlons = self % lonMapping % getIndex(coord % lon)
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


end module shr_gridIndicesMapping_mod 

