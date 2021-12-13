!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :   shr_gridDomainToSquaredConverter_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomainToSquaredConverter_test

	!use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuitegridDomainToSquaredConverter

  type, extends(testSuite) :: testSuitegridDomainToSquaredConverter

  contains
    procedure :: define => defineTestCases
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridDomainToSquaredConverter), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridDomainToSquaredConverter_test