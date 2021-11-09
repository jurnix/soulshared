!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : shr_arrayGridFullRsp_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> common array subroutines 
!------------------------------------------------------------------------------
module shr_arrayGridFullRsp_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp, dp

!  use shr_strings_mod, only: string
!  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayRspDim, shr_arrayDimContainer
!  use SHR_array_mod, only: shr_arrayRsp

  implicit none

  private
  public :: testSuiteArrayGridFullRsp


  type, extends(testSuite) :: testSuiteArrayGridFullRsp
    contains
      procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArrayGridFullRsp), intent(inout) :: self

    call self % assert(.FALSE., "todo = T")

  end subroutine defineTestCases

end module shr_arrayGridFullRsp_test

