!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODUL      : shr_gGridAxesBounds_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridAxesBounds unit tests
!------------------------------------------------------------------------------
module shr_gGridAxesBounds_test
  use SHR_testSuite_mod, only: testSuite
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private
  public :: testSuitegGridAxesBounds

  type, extends(testSuite) :: testSuitegGridAxesBounds

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridAxesBounds), intent(inout) :: self

  end subroutine defineTestCases

end module shr_gGridAxesBounds_test

