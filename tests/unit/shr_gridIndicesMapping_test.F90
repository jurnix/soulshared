!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridIndicesMapping_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridIndicesMapping unit tests
!------------------------------------------------------------------------------
module shr_gridIndicesMapping_test

  use SHR_testSuite_mod, only: testSuite

  use shr_strings_mod, only: string
  use shr_coord_mod, only: shr_coord
  use shr_gridcellIndex_mod, only: shr_gridcellIndex
  use shr_gGridAxes_mod, only: shr_gGridAxes
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gridIndicesMapping_mod, only: shr_gridIndicesMapping

  implicit none

  private
  public :: testSuitegridIndicesMapping

  type, extends(testSuite) :: testSuitegridIndicesMapping

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridIndicesMapping), intent(inout) :: self
    type(shr_gridIndicesMapping) :: m
    type(shr_gGridAxes) :: latAxis
    type(shr_gGridAxes) :: lonAxis

    type(shr_gGridAxesBounds) :: latBounds, lonBounds
    type(string) :: latName, lonName
    type(shr_coord) :: topCenter, center, lastGC
    type(shr_gridcellIndex), allocatable :: foundIndices(:)

    call latBounds % init(1., -1.)
    latname = string("latitude")
    call latAxis % init(latName, 1., latBounds)

    call lonBounds % init(2., 0.)
    lonName = string("longitude")
    call lonAxis % init(lonName, 1., lonBounds)

    call m % init(latAxis, lonAxis)
    call self % assert(.true., "l % init(latAxis(-,-1), lonAxis(2,0)) = T")

    !
    ! grid
    !
    ! (2) (1) (0)
    !  |   |   |
    !  +---+---+-  (1.)
    !  | x | x |
    !  +---+---+-  (0.)
    !  | x | x |
    !  +---+---+- (-1.)
    topCenter = shr_coord(1., 1.)
    foundIndices = m % getIndex(topCenter)
    call self % assert(size(foundIndices) == 2, &
            "l % getIndex(1, 2) size .eq. 2 = T")


    center = shr_coord(0., 1.)
    foundIndices = m % getIndex(center)
    call self % assert(size(foundIndices) == 4, &
            "l % getIndex(0, 1) size .eq. 4 = T")

    lastGC = shr_coord(-0.5, 0.5) 
    foundIndices = m % getIndex(lastGC)
    call self % assert(size(foundIndices) == 1, &
            "l % getIndex(-0.5, 0.5) size .eq. 1 = T")
  end subroutine defineTestCases

end module shr_gridIndicesMapping_test

