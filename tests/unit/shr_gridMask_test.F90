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
  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_gridcellIndex_mod, only: shr_gridcellIndex

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

    type(shr_gGridDescriptor) :: gDescriptor
    logical :: expMask(2,2)
    logical, allocatable :: foundMask(:,:)

    type(shr_gridcellIndex) :: gcIndex


    call latBounds % init(1., -1.)
    latname = string("latitude")
    call latAxis % init(latName, RES, latBounds)

    call lonBounds % init(2., 0.)
    lonName = string("longitude")
    call lonAxis % init(lonName, RES, lonBounds)

    bounds = latAxis % getBounds() * lonAxis % getBounds()

    call gdescriptor % init(RES, bounds, latAxis, lonAxis)

    call m % init(gDescriptor)
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
    call self % assert(m % countEnabled() == 4, "m % count() .eq. 4 = T")

    !< setStatus
    call gcIndex % init(2, 2)
    call m % setStatus(gcIndex, .false.)
    call gcIndex % init(1, 2)
    call m % setStatus(gcIndex, .false.)

    call self % assert(m % countEnabled() == 2, "m % count() .eq. 2 = T")

    !< getRaw
    expMask = .true.
    expMask(1,2) = .false.
    expMask(2,2) = .false.
    foundMask = m % getRaw()
    call self % assert(all(foundMask .eqv. expMask), &
            "m % getRaw() .eq. ((T,F), (T,F)) = T")

    !< get status
    call gcIndex % init(1, 1)
    call self % assert(m % getStatus(gcIndex), &
            "m % getStatus(1,1) = T")

    call gcIndex % init(2, 2)
    call self % assert(.not. m % getStatus(gcIndex), &
            "m % getStatus(1,1) = F")
  end subroutine defineTestCases


end module shr_gridMask_test

