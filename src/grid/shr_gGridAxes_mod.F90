!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridAxes_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridAxes maps which gridcells are enabled or disabled
!> 
!------------------------------------------------------------------------------
module shr_gGridAxes_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_arrayDim_mod, only: shr_arrayRspDim
  use shr_strings_mod, only: string
!  use shr_gridBounds_mod, only: shr_gridBounds
!  use shr_gridcell_mod, only: shr_gridcell

  implicit none

  public :: shr_gGridAxes

  logical, parameter :: ISDEBUG = .false.


  type shr_gGridAxes
    real(kind=sp) :: resolution
    real(kind=sp) :: start, end !< gGridAxesBounds

    class(shr_arrayRspDim), allocatable :: axes 
  contains
    procedure :: init => gGridAxes_initialize 

!    procedure :: getStart
!    procedure :: getEnd
!    procedure :: resolution 
!    procedure :: getSize !< total number of cells
!    procedure :: getIndices !< Given an axes coordinates it return its index
!    procedure :: getGridAxesCell !< Given an index it return its AxesCell info 
  end type shr_gGridAxes

contains

  subroutine gGridAxes_initialize(self, name, startCoord, endCoord, resolution)
    !< gGridAxes initialization
    class(shr_gGridAxes), intent(inout) :: self
    type(string), intent(in) :: name
    real(kind=sp), intent(in) :: resolution !< axes cell width
    real(kind=sp), intent(in) :: startCoord, endCoord

    self % resolution = resolution
    self % start = startCoord
    self % end = endCoord

    allocate(self % axes)
    call self % axes % init(name % toString(), startCoord, endCoord, resolution)
  end subroutine gGridAxes_initialize


end module shr_gGridAxes_mod 

