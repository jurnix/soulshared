!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcellIndex_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> 2D Grid array indices
!> 
!------------------------------------------------------------------------------
module shr_gridcellIndex_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridcell_mod, only: shr_gridcell

  implicit none

  public :: shr_gridcellIndex

  logical, parameter :: ISDEBUG = .false.



  type shr_gridcellIndex
    integer :: idxlat
    integer :: idxlon
  contains
    procedure :: init => gridcellIndex_initialize 
  end type shr_gridcellIndex

contains

  subroutine gridcellIndex_initialize(self, idxlat, idxlon)
    !< gridcellIndex initialization
    class(shr_gridcellIndex), intent(inout) :: self
    integer, intent(in) :: idxlat, idxlon
    self % idxlat = idxlat
    self % idxlon = idxlon
  end subroutine gridcellIndex_initialize


  function getIndices(self) result (indices)
    !< it returns x and y array indices
    class(shr_gridcellIndex), intent(in) :: self
    integer :: indices(2) !< output
    indices(1) = self % idxlat
    indices(2) = self % idxlon
  end function getIndices

end module shr_gridcellIndex_mod 

