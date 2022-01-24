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

  use shr_gGridDescriptor_mod, only: shr_gGridDescriptor, shr_iGGridDescriptor
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
    type(shr_gGridDescriptor) :: d, other, dsimple, doverlap, dother, dd
    class(shr_iGGridDescriptor), allocatable :: idesc
    type(shr_gGridDescriptor) :: combined

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

    ! equal (==)
    call self % assert( d == d, "d .eq. d = T")

    call other % init(1., bounds, latAxis, latAxis)
    call self % assert( .not. (other == d), &
            "bounds(..., lat, lat) .eq. d(..., lat, lon) = T")

    ! simple initalization
    call dsimple % init(1., bounds)
    call self % assert( dsimple == d, &
            "d(1, ..., lat, lon ) .eq. dsimple(1, ...) = T")

    ! combine
    call bounds % init(0., -1., 20., -10.) !< n, s, e, w
    call doverlap % init(1., bounds)
    idesc = d + doverlap
    select type(c => idesc)
    type is (shr_gGridDescriptor)
      combined = c
      call self % assert(combined % getBounds() == [1., -1., 20., -10.], &
          "d(1,-1,2,0) + doverlap(0,-1,20,-10) .eq. [1, -1, 20, -10] = T")
    class default
      call self % assert(.false., &
          "d(1,-1,2,0) + doverlap(0,-1,20,-10) .eq. [1, -1, 20, -10] = T")
    end select


    ! fitsIn
    call bounds % init(10., -10., 20., 0.) !< n, s, e, w
    call dother % init(1., bounds)
    call bounds % init(1., -1., 2., 0.) !< n, s, e, w
    call dd % init(1., bounds)

    call self % assert(dd % fitsIn(dd), &
        "dd(1,-1,2,0) % fitsIn(dd) = T")
    call self % assert(dother % fitsIn(dd), &
        "dother(10,-10,20,0) % fitsIn(dd) = T")
    call self % assert(.not. dd % fitsIn(dother), &
        "dd(10,-10,20,0) % fitsIn(dother) = F")

  end subroutine defineTestCases


end module shr_gGridDescriptor_test

