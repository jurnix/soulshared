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

    real(kind=sp) :: resolution
    type(shr_gGridAxes), allocatable :: latitudes
    type(shr_gGridAxes), allocatable :: longitudes
  contains
    procedure :: init => gridcellsMap_initialize 
    ! procedure :: createGridcellsMapIterator() -> shr_gridcellsMapIterator
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


end module shr_gridcellsMap_mod 

