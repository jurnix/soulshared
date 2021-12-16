!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_mask_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> mask unit tests
!------------------------------------------------------------------------------
module shr_mask_test
  use shr_testSuite_mod, only: testSuite

  use shr_mask_mod, only: shr_mask1d, shr_mask2d
  use shr_maskIndices_mod, only: shr_maskIndices_1d, shr_maskIndices_2d
  !use shr_strings_mod, only: string

  implicit none

  private
  public :: testSuiteMask

  type, extends(testSuite) :: testSuiteMask

  contains
    procedure :: define => defineTestCases
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteMask), intent(inout) :: self

  end subroutine defineTestCases

end module shr_mask_test