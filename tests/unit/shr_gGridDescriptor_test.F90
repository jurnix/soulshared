!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridDescriptor_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridDescriptor unit tests
!------------------------------------------------------------------------------
module shr_gGridDescriptor_test

  use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuitegGridDescriptor

  type, extends(testSuite) :: testSuitegGridDescriptor

  contains
    procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridDescriptor), intent(inout) :: self
    call self % assert(.false., "todo = T")
  end subroutine defineTestCases


end module shr_gGridDescriptor_test

