!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_maskIterator_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_maskIterator unit tests
!------------------------------------------------------------------------------
module shr_maskIterator_test

  use shr_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteMaskIterator

  type, extends(testSuite) :: testSuiteMaskIterator
  contains
    procedure :: define => defineTestCases
  end type testSuiteMaskIterator
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMaskIterator), intent(inout) :: self


  end subroutine defineTestCases

end module shr_maskIterator_test