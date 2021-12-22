!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClustersIterator_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMaskClustersIterator unit tests
!------------------------------------------------------------------------------
module shr_gridMaskClustersIterator_test
  use SHR_testSuite_mod, only: testSuite

  use shr_gridMask_stub, only: shr_gridMaskStub
  use shr_gridMaskClustersIterator_mod, only: shr_gridMaskClustersIterator

  implicit none

  private
  public :: testSuitegridMaskClustersIterator


  type, extends(testSuite) :: testSuitegridMaskClustersIterator
  contains
    procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMaskClustersIterator), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridMaskClustersIterator_test

