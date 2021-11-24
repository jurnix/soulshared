!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridAxesBounds_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridAxesBounds defines an axes bounds 
!> 
!------------------------------------------------------------------------------
module shr_gGridAxesBounds_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp


  implicit none

  public :: shr_gGridAxesBounds

  logical, parameter :: ISDEBUG = .false.


  type shr_gGridAxesBounds
    real(kind=sp) :: start, end 
  contains
    procedure :: init => gGridAxesBounds_initialize 
    procedure :: isIn
    generic, public :: operator(.in.) => isIn
  end type shr_gGridAxesBounds

contains

  elemental subroutine gGridAxesBounds_initialize(self, startCoord, endCoord)
    !< gGridAxesBounds initialization
    class(shr_gGridAxesBounds), intent(inout) :: self
    real(kind=sp), intent(in) :: startCoord, endCoord 
    self % start = startCoord
    self % end = endCoord
  end subroutine gGridAxesBounds_initialize


  elemental logical function isIn(self, axisCoord)
    !< true if 'axisCoord' is inbetween start and end axisBounds
    class(shr_gGridAxesBounds), intent(in) :: self
    real(kind=sp), intent(in) :: axisCoord
    isIn = (axisCoord >= self % start) .and. (axisCoord <= self % end)
  end function isIn


end module shr_gGridAxesBounds_mod 

