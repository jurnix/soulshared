!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : gridPartition_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> grid partitoin unit tests
!------------------------------------------------------------------------------
module shr_gGrid_test
  use shr_testSuite_mod, only: testSuite
  use shr_gGrid_mod, only: shr_gGrid


  implicit none

  private
  public :: testSuiteGGrid

  type, extends(testSuite) :: testSuiteGGrid

    contains
      procedure :: define => defineTestCases
      procedure, private :: testCaseGetIndicesByGridDescriptor
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGGrid), intent(inout) :: self

    !< getIndicesByGridDescriptor
    call self % testCaseGetIndicesByGridDescriptor()
    !< fitsIn_byGrid, fitsIn_byGridDescriptor
    !< combine
    !< equal
    !< toString
  end subroutine defineTestCases


  subroutine testCaseGetIndicesByGridDescriptor(Self)
    !< GetIndicesByGridDescriptor test case
    class(testSuiteGGrid), intent(in) :: self
  end subroutine testCaseGetIndicesByGridDescriptor

end module shr_gGrid_test

