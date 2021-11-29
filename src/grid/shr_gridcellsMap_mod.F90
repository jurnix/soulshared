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
!> 
!> 
!------------------------------------------------------------------------------
module shr_gridcellsMap_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridcell_mod, only: shr_gridcell
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_coord_mod, only: shr_coord


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
    integer, allocatable :: indices(:,:)

    real(kind=sp) :: resolution
    type(shr_gGridAxes), allocatable :: latitudes
    type(shr_gGridAxes), allocatable :: longitudes
  contains
    procedure :: init => gridcellsMap_initialize 
    ! procedure :: createGridcellsMapIterator() -> shr_gridcellsMapIterator

    procedure :: getGridcellsByCoord
    generic :: getGridcells => getGridcellsByCoord

    procedure :: getIndexByGridcell
    generic :: getIndex => getIndexByGridcell
  end type shr_gridcellsMap

contains

  subroutine gridcellsMap_initialize(self, resolution, latAxes, lonAxes)
    !< gridcellsMap initialization
    !<
    class(shr_gridcellsMap), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gGridAxes), intent(in) :: latAxes
    type(shr_gGridAxes), intent(in) :: lonAxes

    self % resolution = resolution
    allocate(self % latitudes, source = latAxes)
    allocate(self % longitudes, source = lonAxes)

    ! populate
    self % gridcells = self % latitudes % expand(self % longitudes)
  end subroutine gridcellsMap_initialize


  function getGridcellsByCoord(self, coord) result (gridcells)
    !< given a coordinate it returns the matching gridcells
    !< From 1 to 4 possible gridcells
    !< coord must be inside the grid bounds
    use shr_gAxisMapping_mod, only: shr_gAxisMapping
    class(shr_gridcellsMap), intent(in) :: self
    type(shr_coord), intent(in) :: coord
    type(shr_gridcell), allocatable :: gridcells(:)
    integer, allocatable :: latIdxs(:), lonIdxs(:)
    integer :: ilat, ilon, icell
    integer :: nlats, nlons
    integer :: gloLatIdx, gloLonIdx

    type(shr_gAxisMapping) :: latMap, lonMap
    !< discover gridcells indices from coord
    call latMap % init(self % latitudes)
    call lonMap % init(self % longitudes)
    latIdxs = latMap % getIndex(coord % lat)
    lonIdxs = lonMap % getIndex(coord % lon)

    nlats = size(latIdxs)
    nlons = size(lonIdxs)

    allocate(gridcells(nlats * nlons))

    icell = 1
    do ilat = 1, nlats
      do ilon = 1, nlons
        gloLatIdx = latIdxs(ilat)
        gloLonIdx = lonIdxs(ilon)
        !< populate output
        gridcells(icell) = self % gridcells(gloLatIdx, gloLonIdx)
        icell = icell + 1
      enddo ! ilon
    enddo ! ilat
  end function getGridcellsByCoord


  integer function getIndexByGridcell(self, gc)
    !< it returns a gridcell index by providing a 'gridcell'
    class(shr_gridcellsMap), intent(in) :: self
    type(shr_gridcell), intent(in) :: gc
  end function getIndexByGridcell


end module shr_gridcellsMap_mod 

