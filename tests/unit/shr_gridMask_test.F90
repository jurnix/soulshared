!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridMask_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridMask unit tests
!------------------------------------------------------------------------------
module shr_gridMask_test

  use shr_precision_mod, only: sp
  use SHR_testSuite_mod, only: testSuite

  use shr_strings_mod, only: string
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gridMask_mod, only: shr_gridMask
  use shr_gridBounds_mod, only: shr_gridBounds

  implicit none

  private
  public :: testSuitegridMask

  type, extends(testSuite) :: testSuitegridMask

  contains
    procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridMask), intent(inout) :: self
    real(kind=sp), parameter :: RES = 1.0
    type(shr_gridMask) :: m
    type(shr_gGridAxes) :: latAxis
    type(shr_gGridAxes) :: lonAxis
    type(shr_gridBounds) :: bounds

    type(shr_gGridAxesBounds) :: latBounds, lonBounds
    type(string) :: latName, lonName

    call latBounds % init(1., -1.)
    latname = string("latitude")
    call latAxis % init(latName, RES, latBounds)

    call lonBounds % init(2., 0.)
    lonName = string("longitude")
    call lonAxis % init(lonName, RES, lonBounds)

    bounds = latAxis % getBounds() * lonAxis % getBounds()

    call m % init(RES, bounds, latAxis, lonAxis)
    call self % assert(.true., "m % init(1., latAxis(1,-1), lonAxis(2,0), ...) = T")

    !
    ! grid
    !
    ! X -> requested coordinates
    !
    ! (2) (1) (0)
    !  |   |   |
    !  +---X---X-  (1.)
    !  |   |   |
    !  +---+---+-  (0.)
    !  |   | X |
    !  +---+---+- (-1.)
    !
    !  mapped into:
    !
    ! +-----+-----+
    ! | 1,1 | 1,2 |
    ! +-----+-----+
    ! | 2,1 | 2,2 |
    ! *-----+-----+
    !
  end subroutine defineTestCases


end module shr_gridMask_test

