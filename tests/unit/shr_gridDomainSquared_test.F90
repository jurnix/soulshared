!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainSquared_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomainSquared_test

	!use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuitegridDomainSquared

  type, extends(testSuite) :: testSuitegridDomainSquared

  contains
    procedure :: define => defineTestCases
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridDomainSquared), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridDomainSquared_test