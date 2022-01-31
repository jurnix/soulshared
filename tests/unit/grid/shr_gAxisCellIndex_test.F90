!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gAxisCellIndex_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gAxisCellIndex unit tests
!------------------------------------------------------------------------------
module shr_gAxisCellIndex_test

  use SHR_testSuite_mod, only: testSuite

  use shr_gAxisCellIndex_mod, only: shr_gAxisCellIndex
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  use shr_gAxisBounds_mod, only: shr_gAxisBounds

  implicit none

  private
  public :: testSuitegAxisCellIndex

  type, extends(testSuite) :: testSuitegAxisCellIndex

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegAxisCellIndex), intent(inout) :: self
    type(shr_gGridAxesCell) :: acell
    type(shr_gAxisCellIndex) :: idxcell 
    type(shr_gAxisBounds) :: bounds

    call bounds % init(2., -2.)
    call acell % init(0., bounds)

    call idxcell % init(2, acell)
    call self % assert(.true., "idxcell % init(2, [2,0,-2]) = T")

    call self % assert(idxcell % getIndex() == 2 &
            , "idxcell % getIndex() .eq. 2 = T")
  end subroutine defineTestCases

end module shr_gAxisCellIndex_test

