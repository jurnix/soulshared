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

    procedure, private :: testCaseSimpleConversion
    procedure, private :: testCaseComplex
  end type
contains

  subroutine defineTestCases(self)
    !< test
    use iso_c_binding
    class(testSuitegridDomainToSquaredConverter), intent(inout) :: self
    call self % testCaseSimpleConversion()
    call self % testCaseComplex()
  end subroutine defineTestCases


  subroutine testCaseSimpleConversion(self)
    class(testSuitegridDomainToSquaredConverter), intent(inout) :: self

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

    type(shr_gridDomainToSquaredConverter) :: converter
    !class(shr_gridDomain) :: domain

    !domain = createNewGridDomain()
    !call converter % init()

    call self % assert(.false., "testCaseSimpleConversion (TODO) = T")
  end subroutine testCaseSimpleConversion


  subroutine testCaseComplex(self)
    class(testSuitegridDomainToSquaredConverter), intent(inout) :: self
    !> (4)   (0)
    !>  - x x -	 (4)				-> 1st - x x -  ->    x x
    !>  x x - -					    -> 2d  x x - -  ->  x x
    !>  - - - -     ->
    !>  x x x x					    -> 3rd x x x x  ->  x x x x
    !>  x x x x  (-1)							 x x x x 			x x x x
    !>
    !> 1st
    !> (3)(1)
    !>  x  x (4)
    !>
    !> 2nd
    !> (4)(3)
    !>  x  x (3)
    !>
    !>   3rd
    !> (4)   (0)
    !>  x x x x  (1)
    !>  x x x x (-1)
    call self % assert(.false., "testCaseComplex (TODO) = T")
  end subroutine testCaseComplex

end module shr_gridDomainToSquaredConverter_test