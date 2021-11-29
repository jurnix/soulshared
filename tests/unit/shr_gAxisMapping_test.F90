!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gAxisMapping_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gAxisMapping unit tests
!------------------------------------------------------------------------------
module shr_gAxisMapping_test

  use SHR_testSuite_mod, only: testSuite
  use shr_strings_mod, only: string
!  use shr_gAxisMapping_mod, only: shr_gAxisMapping
!  use shr_gAxisMappingBounds_mod, only: shr_gAxisMappingBounds

!  use shr_gridcell_mod, only: shr_gridcell
!  use shr_coord_mod, only: shr_coord

  implicit none

  private
  public :: testSuitegAxisMapping

  type, extends(testSuite) :: testSuitegAxisMapping

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegAxisMapping), intent(inout) :: self

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gAxisMapping_test

