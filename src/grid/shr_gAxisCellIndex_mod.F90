!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gAxisCellIndex_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gAxisIndex maps gridAxisCell into an integer matrix
!> 
!------------------------------------------------------------------------------
module shr_gAxisCellIndex_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  
  implicit none

  public :: shr_gAxisCellIndex

  logical, parameter :: ISDEBUG = .false.


  type shr_gAxisCellIndex
    type(shr_gGridAxesCell), allocatable :: cell
    integer :: index
  contains
    procedure :: init => gAxisIndex_initialize 

  end type shr_gAxisCellIndex

contains

  subroutine gAxisIndex_initialize(self, axisCell, index)
    !< gAxisIndex initialization
    class(shr_gAxisCellIndex), intent(inout) :: self
    type(shr_gGridAxesCell), intent(in) :: axisCell
    integer, intent(in) :: index
    allocate(self % cell, source = axisCell)
    self % index = index
  end subroutine gAxisIndex_initialize


  integer function getIndex(self)
    !< returns its index
    class(shr_gAxisCellIndex), intent(in) :: self
    getIndex = self % index
  end function getIndex


  type(shr_gGridAxesCell) function getAxisCell(self)
    !< returns axis cell
    class(shr_gAxisCellIndex), intent(in) :: self
    getAxisCell = self % cell
  end function getAxisCell


end module shr_gAxisCellIndex_mod 

