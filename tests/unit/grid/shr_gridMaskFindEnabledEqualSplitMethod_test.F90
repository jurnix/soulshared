!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClusters_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gridMaskFindEnabledEqualSplitMethod unit tests
!------------------------------------------------------------------------------
module shr_gridMaskFindEnabledEqualSplitMethod_test
  use SHR_testSuite_mod, only: testSuite

  use shr_gridMaskFindEnabledEqualSplitMethod_mod, only: shr_gridMaskFindEnabledEqualSplitMethod


  implicit none

  private
  public :: testSuiteGridMaskFindEnabledEqualMethod

  type, extends(testSuite) :: testSuiteGridMaskFindEnabledEqualMethod
  contains
    procedure :: define => defineTestCases
  end type 

contains


  !<
  !< unit test
  !<
  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGridMaskFindEnabledEqualMethod), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridMaskFindEnabledEqualSplitMethod_test

