!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridMapCoords_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> gGridMapCoords unit tests
!>
!------------------------------------------------------------------------------
module shr_gGridMapCoords_test

  use shr_testSuite_mod, only: testSuite
  use shr_gGridMapCoords_mod, only: shr_gGridMapCoords


  implicit none

  private
  public :: testSuitegGridMapCoords


  type, extends(testSuite) :: testSuitegGridMapCoords
  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseGetByGridIndex
    procedure, private :: testCaseGetByGridBoundIndices
  end type

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridMapCoords), intent(inout) :: self
    call self % testCaseGetByGridIndex()
  end subroutine defineTestCases


  subroutine testCaseGetByGridIndex(self)
    !< toString unit test
    class(testSuitegGridMapCoords), intent(inout) :: self
    call self % assert(.false., "testCaseGetByGridIndex, TODO = T")
  end subroutine testCaseGetByGridIndex


  subroutine testCaseGetByGridBoundIndices(self)
    !< toString unit test
    class(testSuitegGridMapCoords), intent(inout) :: self
    call self % assert(.false., "testCaseGetByGridBoundIndices, TODO = T")
  end subroutine testCaseGetByGridBoundIndices

end module shr_gGridMapCoords_test

