!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : gridcell_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gridcell unit tests
!------------------------------------------------------------------------------
module gridcell_test
  use shr_gridcell_mod, only: gridcell
!  use shr_gridcell_mod, only: GRIDCELL_N_NEAST, GRIDCELL_N_NORTH, GRIDCELL_N_EAST, &
!            GRIDCELL_N_SEAST, GRIDCELL_N_SOUTH, GRIDCELL_N_SWEST, GRIDCELL_N_WEST, &
!            GRIDCELL_N_NWEST
  use shr_coord_mod, only: coord
  use shr_testSuite_mod, only: testSuite
!  use shr_precision_mod, only: sp
  use shr_strings_mod, only: string

  implicit none

  private
  public :: testSuiteGridCell

  type, extends(testSuite) :: testSuiteGridCell

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGridCell), intent(inout) :: self

    type(coord) :: center, cin, cout
    type(gridcell) :: gc, gcBig

    center = coord(2, -1)
    cin = coord(3., 0.)
    cout = coord(10., -10.)
    gc = gridcell(1, 2., center, .true.)
    gcBig = gridcell(2, 3., center, .true.)

    call self % assert(gc == gc, "gc(..) .eq. gc(..) = T")
    call self % assert(.not. (gc == gcBig), "gc(..) .eq. gcBig(..) = F")


!    call self % assert(gc % getSpatialIdxs() == 1, "gc % getSpatialIdxs() .eq. 1 = T")
!    call self % assert(gcBig % getSpatialIdxs() == 2, "gcBig % getSpatialIdxs() .eq. 2 = T")

    call self % assert(gc % getNorth() == 3.0, "gc % getNorth() .eq. 3 = T")
    call self % assert(gc % getSouth() == 1.0, "gc % getSouth() .eq. 1 = T")
    call self % assert(gc % getEast() == 0.0, "gc % getEast() .eq. 0.0 = T")
    call self % assert(gc % getWest() == -2.0, "gc % getWest() .eq. -2.0 = T")


    call self % assert(.not. gc % contains(cout), "gc % contains(cout) = F")
    call self % assert(gc % contains(cin), "gc % contains(cin) = T")
    call self % assert(gc % contains(center), "gc % contains(center) = T")

    call self % assert(gc % toString() == &
            string("(           1) center=    2.0000,   -1.0000, "// &
                   "res=   2.00000000, limits=    3.0000,     1.0000,"// &
                   "     0.0000,    -2.0000"), &
        "gc % toString() .eq. '' = T")

  end subroutine defineTestCases

end module gridcell_test

