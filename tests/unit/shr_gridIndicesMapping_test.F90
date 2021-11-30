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
    procedure :: assertTrueAlloc
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
    type(shr_gridcellIndex), allocatable :: foundIndices(:), expIndices(:)

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
    topCenter = shr_coord(1., 1.)
    allocate(expIndices(2))
    call expIndices(1) % init(1,1)
    call expIndices(2) % init(1,2)
    foundIndices = m % getIndex(topCenter)
    call self % assertTrueAlloc(foundIndices, expIndices, &
            "l % getIndex(1, 2) .eq. ([2,1], [2,2]) = T")
    ! same size?
!    if (size(foundIndices) /= 2) then ! nope 
!      call self % assert(.false., &
!            "l % getIndex(1, 2) size .eq. 2 = T")
!    else ! yes
!      ! same elements?
!      call self % assert(all(foundIndices == expIndices), &
!            "l % getIndex(1, 2) size .eq. ([2,1], [2,2]) = T")
!    endif ! (size(foundIndices) /= 2)
    deallocate(expIndices)


    center = shr_coord(0., 1.)
    allocate(expIndices(4))
    call expIndices(1) % init(1,1)
    call expIndices(2) % init(1,2)
    call expIndices(3) % init(2,1)
    call expIndices(4) % init(2,2)
    foundIndices = m % getIndex(center)
!    call self % assert(size(foundIndices) == 4, &
!            "l % getIndex(0, 1) size .eq. 4 = T")
    call self % assertTrueAlloc(foundIndices, expIndices, &
            "l % getIndex(0, 1) size .eq. ([1,1],[1,2],[2,1],[2,2]) = T")
    deallocate(expIndices)


    lastGC = shr_coord(-0.5, 0.5) 
    allocate(expIndices(1))
    call expIndices(1) % init(2,2)
    foundIndices = m % getIndex(lastGC)
!    call self % assert(size(foundIndices) == 1, &
!            "l % getIndex(-0.5, 0.5) size .eq. 1 = T")
    call self % assertTrueAlloc(foundIndices, expIndices, &
            "l % getIndex(-0.5, 0.5) size .eq. ([2,2]) = T")
    deallocate(expIndices)
  end subroutine defineTestCases


  subroutine assertTrueAlloc(self, expected, found, textMessage)
    !< Assertion wrapper to allocatable arrays
    !< both must be allocated, otherwise false
    !< both must have the same size, otherwise false
    class(testSuitegridIndicesMapping), intent(inout) :: self
    type(shr_gridcellIndex), allocatable, intent(in) :: expected(:)
    type(shr_gridcellIndex), allocatable, intent(in) :: found(:)
    character(*), intent(in) :: textMessage

    if (.not. allocated(expected)) then
      call self % assert(.false., textMessage)
      return
    endif

    if (.not. allocated(found)) then
      call self % assert(.false., textMessage)
      return
    endif

    ! same size?
    if (size(expected) /= size(found)) then ! nope 
      call self % assert(.false., textMessage)
    else ! yes
      ! same elements?
      call self % assert(all(expected == found), textMessage)
    endif ! (size(foundIndices) /= 2)
  end subroutine assertTrueAlloc

end module shr_gridIndicesMapping_test

