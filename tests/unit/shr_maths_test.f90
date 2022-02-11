module shr_maths_test
  use SHR_maths_mod, only: absoluteDivision
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteMaths

  type, extends(testSuite) :: testSuiteMaths
  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseAbsoluteDivision
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMaths), intent(inout) :: self
    call self % testCaseAbsoluteDivision()
  end subroutine defineTestCases


  subroutine testCaseAbsoluteDivision(self)
    !<
    class(testSuiteMaths), intent(inout) :: self
    integer, allocatable :: result(:)

    result = absoluteDivision(11, 4)
    call self % assert(all(result == [3,3,3,2]), &
          "absoluteDivision(11,4) .eq. [3,3,3,2] = T")
  end subroutine testCaseAbsoluteDivision


end module shr_maths_test

