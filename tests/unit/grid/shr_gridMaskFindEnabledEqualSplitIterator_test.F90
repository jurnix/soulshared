!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClusters_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_GridMaskFindEnabledEqualIterator unit tests
!------------------------------------------------------------------------------
module shr_gridMaskFindEnabledEqualSplitIterator_test
  use SHR_testSuite_mod, only: testSuite

  use shr_gridMaskFindEnabledEqualSplitIterator_mod, only: shr_gridMaskFindEnabledEqualSplitIterator


  implicit none

  private
  public :: testSuiteGridMaskFindEnabledEqualIterator

  type, extends(testSuite) :: testSuiteGridMaskFindEnabledEqualIterator
  contains
    procedure :: define => defineTestCases
  end type 

contains


  !<
  !< unit test
  !<
  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGridMaskFindEnabledEqualIterator), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridMaskFindEnabledEqualSplitIterator_test

