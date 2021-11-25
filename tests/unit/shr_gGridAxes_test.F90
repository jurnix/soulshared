!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : gGridAxes_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridAxes unit tests
!------------------------------------------------------------------------------
module shr_gGridAxes_test

  use SHR_testSuite_mod, only: testSuite
  use shr_strings_mod, only: string
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private
  public :: testSuitegGridAxes

  type, extends(testSuite) :: testSuitegGridAxes

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridAxes), intent(inout) :: self
    type(shr_gGridAxes) :: lats
    type(shr_gGridAxesBounds) :: bounds
    type(string) :: latname 
    integer :: sfound !< debug only

    call bounds % init(3., -3.)
    latname = string("latitude")

    call lats % init(latName, 1., bounds)
    call self % assert(.true., "l % init('latitude', 1, [3,-3]) = T")

    ! getName
    call self % assert(lats % getName() == "latitude", &
            "l % getName() .eq. 'latitude' = T")

    ! getResolution
    call self % assert(lats % getResolution() == 1., &
            "l % getResolution() .eq. 1 = T")

    ! getBounds
    call self % assert(lats % getBounds() == [3., -3.], &
            "l % getBounds() .eq. (3, -3) = T")

    ! getSize
    call self % assert(lats % getSize() == 6, &
            "l % getSize() .eq. 6 = T")

  end subroutine defineTestCases

end module shr_gGridAxes_test

