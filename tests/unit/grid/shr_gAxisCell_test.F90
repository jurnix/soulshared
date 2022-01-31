!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODUL      : shr_gAxisCell_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gAxisCell unit tests
!------------------------------------------------------------------------------
module shr_gAxisCell_test
  use SHR_testSuite_mod, only: testSuite
  use shr_gAxisCell_mod, only: shr_gAxisCell
  use shr_gAxisBounds_mod, only: shr_gAxisBounds
  use shr_gridcell_mod, only: shr_gridcell
  use shr_coord_mod, only: shr_coord

  implicit none

  private
  public :: testSuitegAxisCell

  type, extends(testSuite) :: testSuitegAxisCell

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegAxisCell), intent(inout) :: self

    type(shr_gAxisBounds) :: boundary
    type(shr_gAxisCell) :: c, other
    type(shr_gridcell) :: newGc
    type(shr_coord) :: center

    call boundary % init(10., -10.)
    call c % init(0., boundary)

    !procedure :: getBounds
    call self % assert(c % getBounds() == [10., -10.], &
            "c([10, -10], 0) % getBounds() .eq. (10, -10) = T")

    !procedure :: getCenter
    call self % assert(c % getCenter() == 0., &
            "c([10, -10], 0) % getCenter() .eq. 0. = T")

    !procedure :: isIn
    call self % assert(c % isIn(5.) , &
            "c([10, -10], 0) % isIn(5) = T")
    call self % assert(c % isIn(-10.), &
            "c([10, -10], 0) % isIn(-10) = T")
    call self % assert(.not. (c % isIn(20.)), &
            "c([10, -10], 0) % isIn(20) = F")

    !procedure(==) :: equal_gAxisCell
    call boundary % init(60., 40.)
    call other % init(50., boundary)

    call self % assert(c == c, &
            "c([10, -10], 0) .eq. c([10, -10], 0) = T")
    call self % assert(.not. (c == other), &
            "c([10, -10], 0) .eq. o([60, 40], 50) = F")

    ! create gridcell
    newgc = c * other
    center = shr_coord(0., 50.)

    call self % assert(newGc == shr_gridcell(-1, 20., center, .true.), &
            "c * other .eq. gc(1., [10,-1,60,40]) = T")

  end subroutine defineTestCases

end module shr_gAxisCell_test

