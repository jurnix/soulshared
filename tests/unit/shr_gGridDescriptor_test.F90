!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gGridDescriptor_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridDescriptor unit tests
!------------------------------------------------------------------------------
module shr_gGridDescriptor_test

  use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite

  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor
  use shr_strings_mod, only: string
  use shr_gridBounds_mod, only: shr_gridBounds
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private
  public :: testSuitegGridDescriptor

  type, extends(testSuite) :: testSuitegGridDescriptor

  contains
    procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridDescriptor), intent(inout) :: self
    type(shr_gGridDescriptor) :: d

    type(shr_gGridAxes) :: latAxis
    type(shr_gGridAxes) :: lonAxis
    type(shr_gridBounds) :: bounds

    type(shr_gGridAxesBounds) :: latBounds, lonBounds
    type(string) :: latName, lonName


    call latBounds % init(1., -1.)
    latname = string("latitude")
    call latAxis % init(latName, 1., latBounds)

    call lonBounds % init(2., 0.)
    lonName = string("longitude")
    call lonAxis % init(lonName, 1., lonBounds)

    call bounds % init(1., -1., 2., 0.) !< n, s, e, w

    call d % init(1., bounds, latAxis, lonAxis)
    call self % assert(.true., "d(1, ..., [1,-1,2,0]) = T")

    ! getResolution
    call self % assert(d % getResolution() == 1., &
            "d % getResolution() .eq. 1 = T")

    ! getBounds
    call self % assert(d % getBounds() == bounds, &
            "d % getBounds() .eq. [1,-1,2,0] = T")

    ! getLatAxis
    call self % assert(d % getLatAxis() == latAxis, &
            "d % getLatAxis() .eq. latAxis(1,-1) = T")

    ! getLonAxis
    call self % assert(d % getLonAxis() == lonAxis, &
            "d % getLonAxis() .eq. lonAxis(2,0) = T")
  end subroutine defineTestCases


end module shr_gGridDescriptor_test

