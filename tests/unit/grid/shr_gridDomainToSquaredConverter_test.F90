!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :   shr_gridDomainToSquaredConverter_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomainToSquaredConverter_test

	!use shr_precision_mod, only: sp
  use shr_testSuite_mod, only: testSuite
  use shr_gridDomainToSquaredConverter_mod, only: shr_gridDomainToSquaredConverter
  use shr_gridDomain_mod, only: shr_gridDomain

  implicit none

  private
  public :: testSuitegridDomainToSquaredConverter

  type, extends(testSuite) :: testSuitegridDomainToSquaredConverter

  contains
    procedure :: define => defineTestCases
  end type
contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitegridDomainToSquaredConverter), intent(inout) :: self

    type(shr_gridDomainToSquaredConverter) :: toSquareConvert

    !< how to partition(algorithm):
    !<
    !< ---xxx -> 1st 	(xxx)
    !< xxxxxx					(xxxxxx)
    !< xxxxxx -> 2nd	(xxxxxx)
    !< xxxxxx					(xxxxxx)
    !< xxxx-- -> 3rd	(xxxx
    !<
    !<
    !< ---xxx -> 1st
    !< ------
    !< xx--xx -> 2nd(xx), 3rd(xx)
    !< xxxxxx -> 4th
    !< xxx--x -> 5th, 6th
    !< x-x--x -> 7th, 8th, 9th
    !< ------
    !< ------
    !< xxxx-- -> 10th
    !<

    call self % assert(.false., "TODO = T")
  end subroutine defineTestCases

end module shr_gridDomainToSquaredConverter_test