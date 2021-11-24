!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODUL      : shr_gGridAxesCell_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridAxesCell unit tests
!------------------------------------------------------------------------------
module shr_gGridAxesCell_test
  use SHR_testSuite_mod, only: testSuite
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private
  public :: testSuitegGridAxesCell

  type, extends(testSuite) :: testSuitegGridAxesCell

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridAxesCell), intent(inout) :: self

    type(shr_gGridAxesBounds) :: boundary
    type(shr_gGridAxesCell) :: c

    call boundary % init(10., -10.)
    call c % init(0., boundary)
    call self % assert(.false., "test = T")

  end subroutine defineTestCases

end module shr_gGridAxesCell_test

