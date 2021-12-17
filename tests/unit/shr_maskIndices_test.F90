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
    type(shr_maskIndices_1d) :: mi, mdiff

    !procedure :: eq_object => eq_maskIndices_1d
    call mi % init(1, 5)
    call self % assert(mi == mi, "m1(1,5) .eq. mi(1,5) = T")

    call mdiff % init(2, 5)
    call self % assert(.not. (mi == mdiff), "m1(1,5) .eq. midiff(2,5) = F")

    call mdiff % init(1, 6)
    call self % assert(.not. (mi == mdiff), "m1(1,5) .eq. midiff(1,6) = F")

    !procedure :: toString => toString_maskIndices_1d
    call mi % init(1, 5)
    call self % assert(mi % toString() == "(1:5)", "mi(1,5) % toString() .eq. (1:5) = T")
  end subroutine testMaskIndices1d


  subroutine testMaskIndices2d(self)
    !<
    class(testSuiteMaskIndices), intent(inout) :: self
    call self % assert(.false., "todo = T")

    !procedure :: maskIndices_2d_init_by_array
    !procedure :: maskIndices_2d_init_by_1d
    !procedure :: eq_object => eq_maskIndices_2d
    !procedure :: toString => toString_maskIndices_2d

    !procedure :: getRow
    !procedure :: getCol
  end subroutine testMaskIndices2d

end module shr_maskIndices_test