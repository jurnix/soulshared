!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : ncRealDim_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> 
!------------------------------------------------------------------------------
module shr_arrayRealDim_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp

  use shr_arrayDim_mod, only: shr_arrayRealDim
!  use SIO_ncDimBounds_mod, only: ncDimBounds, ncDimBoundsHolder

!  use SIO_ncBoundsArgs_mod, only: ncBoundsArgsReal

  implicit none

  private
  public :: testSuiteArrayRealDim


  type, extends(testSuite) :: testSuiteArrayRealDim

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArrayRealDim), intent(inout) :: self
    class(shr_arrayRealDim), allocatable :: levels
    class(shr_arrayRealDim), allocatable :: logs
!    type(ncDimBoundsHolder), allocatable :: expBounds(:), foundBounds(:)
!    logical :: haveSameSize, haveSameBounds
!    class(ncBoundsArgsReal), allocatable :: args

    allocate(levels, logs)
    call levels % init("temperature", 1., 10., 1.)
    call logs % init("log levels", 1., 5., 1.)

    call self % assert(levels == levels, "levels .eq. levels == T")
    call self % assert(.not. levels == logs, "levels .eq. logs == F")
    call self % assert(levels % isInBounds(5.), "levels % isInBounds(5) == T")
    call self % assert(.not. levels % isInBounds(-1.), "levels % isInBounds(-1) == F")
    call self % assert(.not. levels % isInBounds(11.), "levels % isInBounds(11) == F")

    call self % assert(levels % getIndex(4.5) == 4, "levels % getIndex(4.5) .eq. 4 == T")
    call self % assert(levels % getIndex(10.) == 10, "levels % getIndex(10) .eq. 10 == T")
    call self % assert(levels % getIndex(1.) == 1, "levels % getIndex(1) .eq. 1 == T")

!    allocate(args)
!    allocate(expBounds(1))
!    allocate(expBounds(1) % hold)
!    call expBounds(1) % hold % init("temperature", 1, 10)
!    foundBounds = levels % getBounds(args)

!    if (.not. allocated(foundBounds)) then
!      call self % assert( .false., &
!            "levels % getBounds() .eq. bounds(1,10) == T")

!    else
!      call self % assert( all(foundBounds == expBounds), &
!            "levels % getBounds() .eq. bounds(1,10) == T")
!    endif

  end subroutine defineTestCases

end module shr_arrayRealDim_test

