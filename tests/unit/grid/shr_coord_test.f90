!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : coord_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_coord_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: dp, sp
  use shr_coord_mod, only: shr_coord

  implicit none

  private
  public :: testSuiteCoord


  type, extends(testSuite) :: testSuiteCoord

  contains
    procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteCoord), intent(inout) :: self
    type(shr_coord) :: c

    ! gridcell
    c = shr_coord(initlat=2., initlon=0.)
    call self % assert (c % getLat() == 2., "coord(2,0) % getLat() .eq. 2 == T")

    call self % assert (c % getLon() == 0., "coord(2,0) % getLon() .eq. 0 == T")

    call self % assert (all(c % toArray() == [2., 0.]), &
            "coord(2,0) % getArray() .eq. [2, 0] == T")
    
    call self % assert (c % toString() == &
            "    2.0000,    0.0000", "coord(2,0) % toString() .eq. '    2.0000,    0.0000' == T")

    call self % assert (shr_coord(0., 2.) == shr_coord(0., 2.), &
            "coord(0,2) .eq. coord(0,2) == T")

    call self % assert (.not. shr_coord(0., 2.) == shr_coord(2., 2.), &
            "coord(0,2) .eq. coord(2,2) == F")

  end subroutine defineTestCases


end module shr_coord_test

