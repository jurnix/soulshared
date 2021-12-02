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

  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_GridBounds_mod, only: shr_gridBounds
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

  implicit none

  public :: shr_gridMask

  logical, parameter :: ISDEBUG = .false.


  type shr_gridMask
    !< grid descriptor
    real(kind=sp) :: resolution
    type(shr_gGridAxes), allocatable :: latitudes
    type(shr_gGridAxes), allocatable :: longitudes
    type(shr_gridBounds), allocatable :: bounds

    logical, allocatable :: mask(:,:)
  contains
    procedure :: init => gridMask_initialize

    procedure :: countEnabled
!    procedure :: setAll(status), assignment(=)

!    procedure :: setStatusByMask
    procedure :: setStatusByGridcellIndex
    generic :: setStatus => setStatusByGridcellIndex

!    procedure :: getStatusByMask
    procedure :: getStatusByGridcellIndex
    generic :: getStatus => getStatusByGridcellIndex
  end type shr_gridMask

contains

  subroutine gridMask_initialize(self, resolution, bounds, latAxis, lonAxis)
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    real(kind=sp), intent(in) :: resolution
    type(shr_gGridAxes), intent(in) :: latAxis
    type(shr_gGridAxes), intent(in) :: lonAxis
    type(shr_gridBounds), intent(in) :: bounds

    integer :: nlats, nlons

    self % resolution = resolution
    allocate(self % latitudes, source = latAxis)
    allocate(self % longitudes, source = lonAxis)
    allocate(self % bounds, source = bounds)

    nlats = self % latitudes % getSize()
    nlons = self % longitudes % getSize()
    allocate(self% mask(nlats, nlons))
    self % mask = .true.
  end subroutine gridMask_initialize


  subroutine setStatusByGridcellIndex(self, gcIndex, newStatus)
    !< set a new status for given coordinate 'coord'
    !< coord must be found inside the grid
    class(shr_gridMask), intent(inout) :: self
    type(shr_gridcellIndex), intent(in) :: gcIndex 
    logical, intent(in) :: newStatus
    self % mask(gcIndex % idxlat, gcIndex % idxlon) = newStatus
  end subroutine setStatusByGridcellIndex


  logical function getStatusByGridcellIndex(self, gcIndex)
    !< returns the gridcell status requested by 'coord'
    class(shr_gridMask), intent(in) :: self
    type(shr_gridcellIndex), intent(in) :: gcIndex
    getStatusByGridcellIndex = self % mask(gcIndex % idxlat, gcIndex % idxlon)
  end function getStatusByGridcellIndex


  integer function countEnabled(self)
    !< return the number of enabled grid cells found
    class(shr_gridMask), intent(in) :: self
    countEnabled = count(self % mask) 
  end function countEnabled
end module shr_gridMask_mod

