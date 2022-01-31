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

  use shr_gAxisCell_mod, only: shr_gAxisCell
  
  implicit none

  public :: shr_gAxisCellIndex

  logical, parameter :: ISDEBUG = .false.


  type shr_gAxisCellIndex
    type(shr_gAxisCell), allocatable :: cell
    integer :: index
  contains
    procedure :: init => gAxisIndex_initialize 

    procedure :: getIndex
    procedure :: getAxisCell
  end type shr_gAxisCellIndex

contains

  subroutine gAxisIndex_initialize(self, index, axisCell)
    !< gAxisIndex initialization
    class(shr_gAxisCellIndex), intent(inout) :: self
    type(shr_gAxisCell), intent(in) :: axisCell
    integer, intent(in) :: index
    allocate(self % cell, source = axisCell)
    self % index = index
  end subroutine gAxisIndex_initialize


  integer function getIndex(self)
    !< returns its index
    class(shr_gAxisCellIndex), intent(in) :: self
    getIndex = self % index
  end function getIndex


  type(shr_gAxisCell) function getAxisCell(self)
    !< returns axis cell
    class(shr_gAxisCellIndex), intent(in) :: self
    getAxisCell = self % cell
  end function getAxisCell

end module shr_gAxisCellIndex_mod 

