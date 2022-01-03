!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomain_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomain_test

	!use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuitegridDomain

  type, extends(testSuite) :: testSuitegridDomain

  contains
    procedure :: define => defineTestCases
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridDomain), intent(inout) :: self

    !procedure :: getGridDescriptor
    !procedure :: getMaskGrid
    !procedure :: getMaskBounds

    !procedure :: gridDomain_combine (+)
    !procedure :: filter
    !procedure :: select

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridDomain_test