!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMaskClusters_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMaskClusters unit tests
!------------------------------------------------------------------------------
module shr_gridMaskClusters_test
  use SHR_testSuite_mod, only: testSuite

  use shr_gridMaskClusters_mod, only: shr_gridMaskClusters

  implicit none

  private
  public :: testSuitegridMaskClusters

  type, extends(testSuite) :: testSuitegridMaskClusters

  contains
    procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMaskClusters), intent(inout) :: self
    type(shr_gridMaskClusters) :: c

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases


end module shr_gridMaskClusters_test

