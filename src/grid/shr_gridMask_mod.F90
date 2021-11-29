!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridMask_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gridMask maps which gridcells are enabled or disabled
!>
!------------------------------------------------------------------------------
module shr_gridMask_mod
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_coord_mod, only: shr_coord
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gridcellsMapping_mod, only: shr_gridcellsMapping
!  use shr_gAxesMapping_mod, only: shr_gAxesMapping

  implicit none

  public :: shr_gridMask

  logical, parameter :: ISDEBUG = .false.


  type shr_gridMask
    !< grid descriptor
    real(kind=sp) :: resolution
    type(shr_gGridAxes), allocatable :: latitudes
    type(shr_gGridAxes), allocatable :: longitudes

!    type(shr_gridcellsMapping), allocatable :: gridMap
!    type(shr_gGridIndices), allocatable :: indices(:) 
    logical, allocatable :: mask(:,:)
  contains
    procedure :: init => gridMask_initialize

!    procedure :: setStatusByGridcell
!    generic :: setStatus => setStatusByGridcell
!    procedure :: getStatusByGridcell
!    generic :: getStatus => getStatusByGridcell
  end type shr_gridMask

contains

  subroutine gridMask_initialize(self, resolution, latAxis, lonAxis)
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gGridAxes), intent(in) :: latAxis
    type(shr_gGridAxes), intent(in) :: lonAxis

    integer :: nlats, nlons

    self % resolution = resolution
    allocate(self % latitudes, source = latAxis)
    allocate(self % longitudes, source = lonAxis)

    nlats = self % latitudes % getSize()
    nlons = self % longitudes % getSize()
    allocate(self% mask(nlats, nlons))
    self % mask = .true.

!    allocate(self % gridMap)
!    call self % gridMap % init(resolution, latAxis, lonAxis)
  end subroutine gridMask_initialize


!  subroutine setStatusByGridcell(self, gridcells, newStatus)
    !< set a new status for given coordinate 'coord'
    !< coord must be found inside the grid
!    class(shr_gridMask), intent(inout) :: self
!    type(shr_gridcell), intent(in) :: gridcells
!    logical, intent(in) :: newStatus
!    type(shr_gridcell), allocatable :: gridcells(:) 
!    type(shr_gGridIndices), allocatable :: indices(:) 
    !< discover gridcells
!    gridcells = self % gridMap % getGridcells(coord)
    !< translate from gridcells to indices
!    indices = self % gridIndices % get(gridcells) 
    !< apply
!    call self % setStatusByGridMapIndex(indices) !< elemental
!  end subroutine setStatusByGridcell


!  logical function getStatusByGridcell(self, coord)
    !< returns the gridcell status requested by 'coord'
!    class(shr_gridMask), intent(in) :: self
!    type(shr_coord), intent(in) :: coord
!  end function getStatusByGridcell


!  type(shr_gGridIndices) function discoverIndices(self, gridcell) 
    !< 
!    class(shr_gridMask), intent(inout) :: self
!    type(shr_gridcell), intent(in) :: gridcells
!  end function discoverIndices


end module shr_gridMask_mod

