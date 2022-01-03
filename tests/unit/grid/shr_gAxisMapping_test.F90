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
  use shr_gAxisMapping_mod, only: shr_gAxisMapping

  use shr_strings_mod, only:string
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

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
    type(shr_gAxisMapping) :: axisMapping, revMapping

    type(string) :: latname
    type(shr_gGridAxes) :: lats, revLats
    type(shr_gGridAxesBounds) :: bounds, revBounds, lastCellBounds
    type(shr_gGridAxesCell) :: lastAxisCell
    integer, allocatable :: idx(:)
    integer :: gcidx !< expected 

    call bounds % init(2., -2.)
    latname = string("latitude")
    ! cells bounds: 2, 1, 0, -1, -2
    !       gcells:   x  x  x   x
    !   array idxs:   1  2  3   4
    call lats % init(latName, 1., bounds)

    call axisMapping % init(lats)
    call self % assert(.true., "axisMapping % init(...) = T")
    
    !procedure :: getIndexByCoord
    idx = axisMapping % getIndexByCoord(-1.5)
    call self % assert(all(idx == [4]), &
          "axisMapping % getIndex(-1.5) .eq. 4 = T")

    idx = axisMapping % getIndexByCoord(-1.)
    !< same size?
    if (size(idx) /= 2) then ! nope
      call self % assert(.false., &
          "axisMapping % getIndexByCoord(-1.) .eq. (3,4) = T")
    else !yes, then check values
      call self % assert(all(idx == [3, 4]), &
          "axisMapping % getIndexByCoord(-1.) .eq. (3,4) = T")
    endif

    !procedure :: getIndexByGridAxisCell
    call lastCellBounds % init(-1., -2.)
    call lastAxisCell % init(-1.5, lastCellBounds)

    gcidx = axisMapping % getIndexByGridAxisCell(lastAxisCell)
    call self % assert(gcidx == 4, &
        "axisMapping % getIndexByGridAxisCell(axisCell) .eq. 4 = T")

    ! 
!    call revbounds % init(-2., 2.) !< not allowed
!    call revLats % init(latName, 1., revBounds)
!    call revMapping % init(lats)
!    call self % assert(.true., "revMapping % init(...) = T")
  end subroutine defineTestCases

end module shr_gAxisMapping_test

