!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskIterator_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMaskIterator unit tests
!------------------------------------------------------------------------------
module shr_gridMaskIterator_test
  use SHR_testSuite_mod, only: testSuite

  use shr_gridMask_stub, only: shr_gridMaskStub
  use shr_gridMaskIterator_mod, only: shr_gridMaskIterator

  implicit none

  private
  public :: testSuitegridMaskIterator


  type, extends(testSuite) :: testSuitegridMaskIterator
  contains
    procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMaskIterator), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridMaskIterator_test

