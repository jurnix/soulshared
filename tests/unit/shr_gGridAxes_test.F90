!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : gGridAxes_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridAxes unit tests
!------------------------------------------------------------------------------
module shr_gGridAxes_test
  use SHR_testSuite_mod, only: testSuite
  use shr_gGridAxes_mod, only: shr_gGridAxes

  implicit none

  private
  public :: testSuitegGridAxes

  type, extends(testSuite) :: testSuitegGridAxes

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridAxes), intent(inout) :: self


  end subroutine defineTestCases

end module shr_gGridAxes_test

