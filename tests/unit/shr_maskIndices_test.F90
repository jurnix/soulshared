!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_maskIndices_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_maskIndices unit tests
!------------------------------------------------------------------------------
module shr_maskIndices_test

  use shr_testSuite_mod, only: testSuite
  use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d

  implicit none

  private
  public :: testSuiteMaskIndices

  type, extends(testSuite) :: testSuiteMaskIndices

  contains
    procedure :: define => defineTestCases
    procedure :: testMaskIndices1d
    procedure :: testMaskIndices2d
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMaskIndices), intent(inout) :: self

    call self % testMaskIndices1d()
    call self % testMaskIndices2d()
  end subroutine defineTestCases


  subroutine testMaskIndices1d(self)
    !<
    class(testSuiteMaskIndices), intent(inout) :: self
    call self % assert(.false., "todo = T")
  end subroutine testMaskIndices1d


  subroutine testMaskIndices2d(self)
    !<
    class(testSuiteMaskIndices), intent(inout) :: self
    call self % assert(.false., "todo = T")
  end subroutine testMaskIndices2d

end module shr_maskIndices_test