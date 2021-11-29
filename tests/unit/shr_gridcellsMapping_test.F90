!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcellsMapping_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridcellsMapping unit tests
!------------------------------------------------------------------------------
module shr_gridcellsMapping_test

  use SHR_testSuite_mod, only: testSuite

  use shr_gridcellsMapping_mod, only: shr_gridcellsMapping
!  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
!  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private
  public :: testSuitegridcellsMapping

  type, extends(testSuite) :: testSuitegridcellsMapping

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridcellsMapping), intent(inout) :: self
    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridcellsMapping_test

