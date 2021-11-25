!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODUL      : shr_gGridAxesBounds_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> gGridAxesBounds unit tests
!------------------------------------------------------------------------------
module shr_gGridAxesBounds_test
  use SHR_testSuite_mod, only: testSuite
  use shr_gGridAxesBounds_mod, only: shr_gGridAxesBounds
  use shr_gridBounds_mod, only: shr_gridBounds

  implicit none

  private
  public :: testSuitegGridAxesBounds

  type, extends(testSuite) :: testSuitegGridAxesBounds

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegGridAxesBounds), intent(inout) :: self

    type(shr_gGridAxesBounds) :: lats, lons
    type(shr_gridBounds) :: gridBounds

    call lats % init(90., -90.)
    call lons % init(180., -180.)
    
    ! getStart
    call self % assert(lats % getStart() == 90., "l(90, -90) % getStart() .eq. 90 = T")
    ! getEnd
    call self % assert(lats % getEnd() == -90., "l(90, -90) % getEnd() .eq. -90 = T")

    ! isIn (axes coord)
    call self % assert(lats % isIn(90.), "l(90, -90) % isIn(90) = T")
    call self % assert(lats % isIn(-90.), "l(90, -90) % isIn(-90) = T")
    call self % assert(lats % isIn(0.), "l(90, -90) % isIn(0) = T")
    call self % assert(.not. lats % isIn(100.), "l(90, -90) % isIn(100) = T")
    call self % assert(.not. lats % isIn(-100.), "l(90, -90) % isIn(-100) = T")

    ! isIn (gGridAxesBounds)
    call self % assert(.not. lats % isIn(lons), "lats(90, -90) % isIn(lons(180,-180)) = F")
    call self % assert(lons % isIn(lats), "lons(180, -180) % isIn(lats(90,-90)) = T")
    call self % assert(lats % isIn(lats), "lats(90, -90) % isIn(lats) = T")

    ! == (gGridAxesBounds vs gGridAxesBounds)
    call self % assert( lats == lats, "l(90, -90) .eq. l(90, -90) = T")
    call self % assert(.not. (lats == lons), "l(90, -90) .eq. l(180, -180) = F")

    ! == (gGridAxesBounds vs array(start, end))
    call self % assert( lats == [90., -90.], "l(90, -90) .eq. (90, -90) = T")
    call self % assert(.not. (lats == [180., -90.]), "l(90, -90) .eq. (180, -90) = F")

    ! * (combine)
    gridBounds = lats * lons
    call self % assert(gridBounds == shr_gridBounds(90., -90., 180., -180.), &
            "lats(90, -90) * lons(180, -180) .eq. gridBunds(...) = F")

  end subroutine defineTestCases

end module shr_gGridAxesBounds_test

