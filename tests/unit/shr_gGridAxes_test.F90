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
  use shr_strings_mod, only: string
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

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
    type(shr_gGridAxes) :: lats
    type(shr_gGridAxesBounds) :: bounds
    type(string) :: latname

    call bounds % init(10., -10.)
    latname = string("latitude")

    call lats % init(latName, 1., bounds)
    call self % assert(.false., "todo = T")

  end subroutine defineTestCases

end module shr_gGridAxes_test

