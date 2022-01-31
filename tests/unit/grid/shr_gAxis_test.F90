!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : gGridAxes_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridAxes unit tests
!------------------------------------------------------------------------------
module shr_gAxis_test

  use SHR_testSuite_mod, only: testSuite
  use shr_strings_mod, only: string
  use shr_gAxis_mod, only: shr_gAxis
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds

  use shr_gridcell_mod, only: shr_gridcell
  use shr_coord_mod, only: shr_coord

  implicit none

  private
  public :: testSuitegAxis

  type, extends(testSuite) :: testSuitegAxis

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegAxis), intent(inout) :: self
    type(shr_gAxis) :: lats, lons
    type(shr_gGridAxesBounds) :: bounds, lonBounds
    type(string) :: latname, lonName 
    type(shr_gridcell), allocatable :: gridcells(:,:)
    integer, parameter :: EMPTY_GID = -1 !< undefined global gridcell id
    type(shr_coord) :: center

    call bounds % init(1., -1.)
    latname = string("latitude")

    call lats % init(latName, 1., bounds)
    call self % assert(.true., "l % init('latitude', 1, [1,-1]) = T")

    ! getName
    call self % assert(lats % getName() == "latitude", &
            "l % getName() .eq. 'latitude' = T")

    ! getResolution
    call self % assert(lats % getResolution() == 1., &
            "l % getResolution() .eq. 1 = T")

    ! getBounds
    call self % assert(lats % getBounds() == [1., -1.], &
            "l % getBounds() .eq. (1, -1) = T")

    ! getSize
    call self % assert(lats % getSize() == 2, &
            "l % getSize() .eq. 2 = T")

    ! expand
    !
    !(10) (9)  (8)  
    ! |    |    |
    ! +----+----+-  ( 1)
    ! |  x |  x | 
    ! +----+----+-- ( 0)
    ! |  x |  x |
    ! +----+----+-  (-1)
    call lonBounds % init(10., 8.)
    lonName = string("longitude")
    call lons % init(lonName, 1., lonBounds)

    gridcells = lats % expand (lons)
    call self % assert(size(gridcells) == 4, &
            "lats % expand(lons) % size .eq. 4 = T")

    call self % assert(size(gridcells, dim=1) == 2, &
            "lats % expand(lons) % size(dim=1) .eq. 2 = T")

    call self % assert(size(gridcells, dim=2) == 2, &
            "lats % expand(lons) % size(dim=2) .eq. 2 = T")

    !< first (1,1)
    center = shr_coord(0.5, 9.5) 
    call self % assert(gridcells(1,1) == shr_gridcell(EMPTY_GID, 1., center, .true.), &
            "lats % expand(lons)[1,1]  .eq. gridcell(1, [0.5, 9.5]) = T")

    !< last (2,2)
    center = shr_coord(-0.5, 8.5) 
    call self % assert(gridcells(2,2) == shr_gridcell(EMPTY_GID, 1., center, .true.), &
            "lats % expand(lons)[2,2]  .eq. gridcell(1, [-0.5, 8.5]) = T")


    !< equal (==)
    call self % assert(lats == lats, "lats .eq. lats = T")
    call self % assert(.not. (lats == lons), "lats .eq. lons = F")

  end subroutine defineTestCases

end module shr_gAxis_test

