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

  use shr_arrayDim_mod, only: shr_arrayRspDim, shr_arrayDimContainer

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
    class(shr_arrayRspDim), allocatable :: levels
    class(shr_arrayRspDim), allocatable :: logs

    class(shr_arrayRspDim), allocatable :: logsSame
    class(shr_arrayRspDim), allocatable :: levelsCopied

    !< array wrapper
    type(shr_arrayDimContainer) :: dimensions(2)
    type(shr_arrayDimContainer) :: newDimensions(2)

    allocate(levels, logs)
    allocate(logsSame, levelsCopied)
    call levels % init("temperature", 1., 10., 1.)
    call logs % init("log levels", 1., 5., 1.)
    call logsSame % init("log levels", 1., 5., 1.)

    call self % assert(levels == levels, "levels .eq. levels == T")
    call self % assert(.not. levels == logs, "levels .eq. logs == F")
    call self % assert(levels % isInBounds(5.), "levels % isInBounds(5) == T")
    call self % assert(.not. levels % isInBounds(-1.), "levels % isInBounds(-1) == F")
    call self % assert(.not. levels % isInBounds(11.), "levels % isInBounds(11) == F")

    call self % assert(levels % getIndex(4.5) == 4, "levels % getIndex(4.5) .eq. 4 == T")
    call self % assert(levels % getIndex(10.) == 10, "levels % getIndex(10) .eq. 10 == T")
    call self % assert(levels % getIndex(1.) == 1, "levels % getIndex(1) .eq. 1 == T")

    ! copy
    levelsCopied = levels
    call self % assert(levelsCopied == levels, "levels .eq. levelsCopied == T")

    ! equal
    call self % assert(.not. levels == logs, "levels .eq. logs == F")
    call self % assert( logsSame == logs, "logsSame .eq. logs == T")

    ! array dimensions
    allocate(dimensions(1) % arrayDim, source = levels)
    allocate(dimensions(2) % arrayDim, source = logs)

    newDimensions = dimensions
    call self % assert(all(newDimensions == dimensions), "newDimensions .eq. dimensions = T")
  end subroutine defineTestCases

end module shr_arrayRealDim_test

