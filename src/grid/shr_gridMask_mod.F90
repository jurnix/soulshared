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
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
!  use shr_GridBounds_mod, only: shr_gridBounds
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

  implicit none

  public :: shr_gridMask

  logical, parameter :: ISDEBUG = .false.


  type shr_gridMask
    !< grid descriptor
    type(shr_gGridDescriptor), allocatable :: gridDescriptor

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

    procedure :: getRaw
  end type shr_gridMask

contains

  subroutine gridMask_initialize(self, gridDescriptor) 
    !< gridMask initialization
    class(shr_gridMask), intent(inout) :: self
    type(shr_gGridDescriptor), intent(in) :: gridDescriptor 

    type(shr_gGridAxes) :: axis
    integer :: nlats, nlons

    allocate(self % gridDescriptor, source = gridDescriptor) 

    axis = self % gridDescriptor % getLatAxis() !latitudes % getSize()
    nlats = axis % getSize()
    axis = self % gridDescriptor % getLonAxis() !longitudes % getSize()
    nlons = axis % getSize()

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


  function getRaw(self) result (outMask)
    !< returns current mask
    class(shr_gridMask), intent(in) :: self
    logical, allocatable :: outMask(:,:) !< output
    allocate(outMask, source = self % mask)
  end function getRaw
end module shr_gridMask_mod

