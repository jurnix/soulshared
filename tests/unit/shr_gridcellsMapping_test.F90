!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridcellsMapping_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridcellsMapping unit tests
!------------------------------------------------------------------------------
module shr_gridcellsMapping_test

  use SHR_testSuite_mod, only: testSuite

  use shr_gridcellsMapping_mod, only: shr_gridcellsMapping

  use shr_gridcell_mod, only: shr_gridcell
  use shr_coord_mod, only: shr_coord
  use shr_strings_mod, only: string
  use shr_gGridAxes_mod, only: shr_gGridAxes
!  use shr_gGridAxesCell_mod, only: shr_gGridAxesCell
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  implicit none

  private
  public :: testSuitegridcellsMapping

  type, extends(testSuite) :: testSuitegridcellsMapping

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridcellsMapping), intent(inout) :: self

    type(shr_gridcellsMapping) :: m
    type(shr_gGridAxes) :: latAxis
    type(shr_gGridAxes) :: lonAxis

    type(shr_gGridAxesBounds) :: latBounds, lonBounds
    type(string) :: latName, lonName
    type(shr_coord) :: topCenter, center, lastGC
    type(shr_coord) :: gcCenter

    type(shr_gridcell), allocatable :: foundGcsTopCenter(:), &
            foundGcsCenter(:), foundGcsLast(:)
    type(shr_gridcell) :: expGcsTopCenter(2), expGcsCenter(4), &
            expGcsLast(1)

    call latBounds % init(1., -1.)
    latname = string("latitude")
    call latAxis % init(latName, 1., latBounds)

    call lonBounds % init(2., 0.)
    lonName = string("longitude")
    call lonAxis % init(lonName, 1., lonBounds)

    call m % init(1., latAxis, lonAxis)
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

    ! topcenter
    gcCenter = shr_coord(0.5, 1.5)
    expGcsTopCenter(1) = shr_gridcell(-1, 1., gcCenter, .true.)
    gcCenter = shr_coord(0.5, 0.5)
    expGcsTopCenter(2) = shr_gridcell(-1, 1., gcCenter, .true.)

    topCenter = shr_coord(1., 1.)
    foundGcsTopCenter = m % get(topCenter)
    call self % assertTrue(foundGcsTopCenter, expGcsTopCenter, &
            "m % get(1,1) .eq. gcs([0.5,1.5], [0.5,0.5]) = T")

    ! center
    gcCenter = shr_coord(0.5, 1.5)
    expGcsCenter(1) = shr_gridcell(-1, 1., gcCenter, .true.)
    gcCenter = shr_coord(0.5, 0.5)
    expGcsCenter(2) = shr_gridcell(-1, 1., gcCenter, .true.)
    gcCenter = shr_coord(-0.5, 1.5)
    expGcsCenter(3) = shr_gridcell(-1, 1., gcCenter, .true.)
    gcCenter = shr_coord(-0.5, 0.5)
    expGcsCenter(4) = shr_gridcell(-1, 1., gcCenter, .true.)

    Center = shr_coord(0., 1.)
    foundGcsCenter = m % get(center)
    call self % assertTrue(foundGcsCenter, expGcsCenter, &
            "m % get(1,1) .eq. gcs([0.5,1.5], [0.5,0.5]) = T")

    ! lastGC
    gcCenter = shr_coord(-0.5, 0.5)
    expGcsLast(1) = shr_gridcell(-1, 1., gcCenter, .true.)

    lastGC = shr_coord(-0.5, 0.5)
    foundGcsLast = m % get(lastGC)
    call self % assertTrue(foundGcsLast, expGcsLast, &
            "m % get(1,1) .eq. gcs([0.5,1.5], [0.5,0.5]) = T")
    
  end subroutine defineTestCases

end module shr_gridcellsMapping_test

