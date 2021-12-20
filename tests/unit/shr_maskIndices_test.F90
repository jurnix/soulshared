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
    type(shr_maskIndices_2d) :: m2, mother
    type(shr_maskIndices_1d) :: cols, rows

    !procedure :: maskIndices_2d_init_by_array
    call m2 % init([3,0,4,1])
    call self % assert(.true., "m2 % init(3,0,4,1) = T")

    !procedure :: maskIndices_2d_init_by_1d
    call cols % init(3,0)
    call cols % init(4,1)
    call m2 % init(cols, rows)
    call self % assert(.true., "m2 % init(cols, rows) = T")

    !procedure :: eq_object => eq_maskIndices_2d
    !< same variable
    call m2 % init([3,0,4,1])
    call self % assert(m2 == m2, "m2 .eq. m2 = T")

    ! same attributes but different variables
    call m2 % init([3,0,4,1])
    call cols % init(3,0)
    call rows % init(4,1)
    call mother % init(cols, rows)
    call self % assert(m2 == mother, "m2(3,0,4,1) .eq. mOther(cols(3,0), rows(4,1)) = T")

    !< different attributes
    call m2 % init([3,0,4,1])
    call mother % init([3,1,4,1])
    call self % assert(.not. (m2 == mother), "m2(3,0,4,1) .eq. mOther(3,1,4,1) = T")

    !procedure :: toString => toString_maskIndices_2d
    call m2 % init([3,0,4,1])
    call self % assert(m2 % toString() == "[(3:0), (4:1)]", &
            "m2(3,0,4,1) % toString() = T")

    !procedure :: getRow
    call cols % init(3,0)
    call rows % init(4,1)
    call mother % init(cols, rows)
    call self % assert(mother % getRow() == rows, &
        "mOther(3,0,4,1) % getRow() .eq. rows(4,1) = T")

    !procedure :: getCol
    call cols % init(3,0)
    call rows % init(4,1)
    call mother % init(cols, rows)
    call self % assert(mother % getCol() == cols, &
        "mOther(3,0,4,1) % getRow() .eq. cols(3,0) = T")
  end subroutine testMaskIndices2d

end module shr_maskIndices_test