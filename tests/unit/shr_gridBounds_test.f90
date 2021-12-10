!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridBounds_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gridBounds unit tests
!------------------------------------------------------------------------------
module shr_gridBounds_test
  !use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite

  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_coord_mod, only: shr_coord

  implicit none

  private
  public :: testSuiteGridBounds

  type, extends(testSuite) :: testSuiteGridBounds

  contains
    procedure :: define => defineTestCases
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGridBounds), intent(inout) :: self

    type(shr_gridBounds) :: b, big, small, outb, outTouched
    type(shr_gridBounds) :: newBigBounds, newSmallBounds

    type(shr_coord) :: cin, cout

    !procedure :: initFromArray
    !procedure :: initGridBounds

    cin = shr_coord(1.5, -0.25)
    cout = shr_coord(5.0, 0.)

    call big % init(4., 0., 2., -1.)
    call small % init(1., 0., 1., 0.)
    call b % init(3., 0., 1., -1.) !< north, south, east, west
    call self % assert(.true., "g % init(3, 0, 1, -1) = T")

    !procedure :: toArray
    call self % assert(all(b % toArray() == [3., 0., 1. ,-1.]), &
            "b % toArray() .eq. [3,0,1,-1] = T")

    !procedure :: toString
    write(*,*) "'", b % toString(), "'"
    call self % assert(b % toString() == "3.0000, 0.0000, 1.0000, -1.0000", &
            "b % toString() .eq. '3, 0, 1, -1' = T")

    !procedure :: fitsCoord
    call self % assert(b % fitsCoord(cin), &
        "b(3,0,1,-1) % fitsCoord(1.5, -0.25) = T")
    call self % assert(.not. b % fitsCoord(cout), &
        "b(3,0,1,-1) % fitsCoord(5, 0) = F")

    !procedure :: fitsGridBounds
    call self % assert(.not. b % fitsGridBounds(big), &
              "b(3,0,1,-1) % fitsGridBounds() .eq. bounds(4,0,2,-1) = F")
    call self % assert(b % fitsGridBounds(small), &
              "b(3,0,1,-1) % fitsGridBounds() .eq. bounds(1,0,1,0) = T")

    !procedure, pass(w0), private :: gridBounds_eq_array
    call self % assert(b == [3., 0., 1., -1.], &
              "b(3,0,1,-1) .eq. [3,0,1,-1] = T")

    !procedure, pass(w0), private :: gridBounds_eq
    call self % assert(b == b, "b(3,0,1,-1) .eq. b(3,0,1,-1) = T")
    call self % assert(.not. b == big, "b(3,0,1,-1) .eq. big(4,0,2,-1) = F")

    !procedure :: gridBounds_combine
    newBigBounds = b + big
    newSmallBounds = b + small
    write(*,*) newBigBounds % toString()
    call self % assert(newBigBounds == [4., 0., 2., -1.], &
              "b(3,0,1,-1) + big(4,0,2,-1) .eq. (4,0,2,-1) = T")
    write(*,*) newSmallBounds % toString()
    call self % assert(newSmallBounds == [3., 0., 1., -1.], &
        "b(3,0,1,-1) + small(1,0,1,0) .eq. (3,0,1,-1) = T")

    ! isOverlapped
    !call b % init(3., 0., 1., -1.) !< north, south, east, west
    !call big % init(4., 0., 2., -1.)
    !call small % init(1., 0., 1., 0.)

    call outb % init(10.,5.,2.,1.)
    call outTouched % init(4.,3.,1.,-1.)
    call self % assert(b % isOverlapped(big), &
            "b(3,0,1,-1) % isOverlapped(4,0,2,-1) = T")
    call self % assert(b % isOverlapped(small), &
        "b(3,0,1,-1) % isOverlapped(1,0,1,0) = T")
    call self % assert(.not. b % isOverlapped(outb), &
        "b(3,0,1,-1) % isOverlapped(10,5,2,1) = F")
    call self % assert(.not. b % isOverlapped(outTouched), &
        "b(3,0,1,-1) % isOverlapped(4,3,1,-1) = F")

  end subroutine defineTestCases

end module shr_gridBounds_test