!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcellsMapping_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> shr_gridcellsMappingping computes coordinates into gridcells 
!> 
!------------------------------------------------------------------------------
module shr_gridcellsMapping_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_coord_mod, only: shr_coord
  use shr_gridcell_mod, only: shr_gridcell
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gAxisMapping_mod, only: shr_gAxisMapping
  use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor


  implicit none

  public :: shr_gridcellsMapping

  logical, parameter :: ISDEBUG = .false.


  type shr_gridcellsMapping
    type(shr_gridcell), allocatable :: gridcells(:,:)

    class(shr_iGGridDescriptor), allocatable :: gDescriptor
    type(shr_gAxisMapping), allocatable :: latMapping
    type(shr_gAxisMapping), allocatable :: lonMapping
  contains
    procedure :: gridcellsMapping_initialize_byDescriptor
    generic :: init => gridcellsMapping_initialize_byDescriptor

    procedure :: getGridcellsByCoord
    generic :: get => getGridcellsByCoord
  end type shr_gridcellsMapping

contains

  subroutine gridcellsMapping_initialize_byDescriptor(self, gDescriptor)
    !< gridcellsMapping initialization
    class(shr_gridcellsMapping), intent(inout) :: self
    class(shr_iGGridDescriptor), intent(in) :: gDescriptor

    type(shr_gGridAxes) :: latAxis
    type(shr_gGridAxes) :: lonAxis
    real(kind=sp) :: res

    allocate(self % gDescriptor, source =  gDescriptor)

    latAxis = gDescriptor % getLatAxis()
    lonAxis = gDescriptor % getLonAxis()
    res = gDescriptor % getResolution()

    ! gridcells mapping indices
    allocate(self % latMapping)
    call self % latMapping % init(latAxis)
    allocate(self % lonMapping)
    call self % lonMapping % init(lonAxis)

    ! populate gridcells
    self % gridcells = latAxis % expand(lonAxis)
  end subroutine gridcellsMapping_initialize_byDescriptor


  function getGridcellsByCoord(self, coord) result (gcells)
    !< given a coordinate it returns the matching indices 
    !< From 1 to 4 possible  
    !< coord must be inside the grid bounds
    use shr_gAxisMapping_mod, only: shr_gAxisMapping
    class(shr_gridcellsMapping), intent(in) :: self
    type(shr_coord), intent(in) :: coord
    type(shr_gridcell), allocatable :: gcells(:)

    integer, allocatable :: idxlats(:), idxlons(:) 
    integer :: ilat, ilon
    integer :: nlats, nlons
    integer :: icell

    !< calculate indices 
    idxlats = self % latMapping % getIndex(coord % lat)
    idxlons = self % lonMapping % getIndex(coord % lon)
    nlats = size(idxlats)
    nlons = size(idxlons)

    allocate(gcells(nlats * nlons))

    ! populate output
    icell = 1
    do ilat = 1, nlats
      do ilon = 1, nlons
        gcells(icell) = self % gridcells(idxlats(ilat), idxlons(ilon))
        icell = icell + 1 ! next
      enddo ! nlons
    enddo ! nlats
  end function getGridcellsByCoord

end module shr_gridcellsMapping_mod 

