!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridIndicesMapping_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridIndicesMapping unit tests
!------------------------------------------------------------------------------
module shr_gridIndicesMapping_test

  use SHR_testSuite_mod, only: testSuite

  use shr_gridIndicesMapping_mod, only: shr_gridIndicesMapping

  implicit none

  private
  public :: testSuitegridIndicesMapping

  type, extends(testSuite) :: testSuitegridIndicesMapping

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridIndicesMapping), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridIndicesMapping_test

