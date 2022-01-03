!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : shr_arrayGridFullRsp_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> arrayGridFull array subroutines 
!------------------------------------------------------------------------------
module shr_arrayGridFullRsp_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp, dp

  use shr_arrayDim_mod, only: shr_arrayRspDim, shr_arrayDimContainer
  use SHR_arrayGridFull_mod, only: shr_arrayGridFullRsp
  use shr_grid_mod, only: shr_grid

  implicit none

  private
  public :: testSuiteArrayGridFullRsp


  type, extends(testSuite) :: testSuiteArrayGridFullRsp
    contains
      procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArrayGridFullRsp), intent(inout) :: self

    type(shr_grid) :: grid
    type(shr_arrayGridFullRsp) :: temperature, incTemp, pressure
    class(shr_arrayRspDim), allocatable :: levels

    real(kind=sp), parameter :: limits(4) = [3, -1, 2, -2] !< N, S, E, W
    real(kind=sp), parameter :: resolution = 2.0
    integer, parameter :: curPartition = 0
    integer, parameter :: nPartitions = 1
    type(shr_arrayDimContainer), allocatable :: tempDims(:)

    real(kind=sp) :: data(3,3)
    real(kind=sp), allocatable :: foundData(:,:)

    data(:,:) = 10.

    !
    ! land       coordinates (center)   global indices 
    !
    ! x x   ->      (2,-1), (2, 1)   ->   1, 2 
    ! x x   ->      (0,-1), (0, 1)   ->   3, 4
    !
    !
    grid = shr_grid(limits, resolution, curPartition, nPartitions)

    allocate(levels)
    call levels % init("levels", 1., 10., 1.)

    call incTemp % init("incTemp", grid, tempDims, &
            "Kelvin", "Increase in air temperature at L levels")
    call temperature % init("temperature", grid, tempDims, &
            "Kelvin", "Air temperature at L levels")
    call self % assert(.true., "temperature % init() = T")

    incTemp = 3.0
    temperature = 274.0
    call self % assert(.true., "temperature copy 274.0 = T")

    call self % assert(temperature == 274.0, "temperature .eq. 274 = T")
    call self % assert(.not. (temperature == data), "temperature .eq. data = F")

    temperature = data
    call self % assert(.not. temperature == 274.0, "temperature .eq. 274 = F")
    call self % assert(temperature == data, "temperature .eq. data = T")


    call self % assert(.not. temperature == incTemp, "temperature .eq. incTemp = F")
    incTemp = temperature
    call self % assert(temperature == incTemp, "temperature .eq. incTemp = T")

    foundData = temperature
    call self % assert(all(foundData == data), "foundData .eq. data = T")

    call pressure % init("pressure", grid, tempDims, &
            "Q", "Air pressure")
    pressure = temperature
    call self % assert(.true., "temperature .copy. pressure = T")

    ! add scalar
    pressure = 300.0
    pressure = pressure + 1.0_sp
    call self % assert(pressure == 301.0, "temperature(300) + 1.0 .eq. 301 = T")

    ! add raw array
    data = 2.0
    pressure = pressure + data
    call self % assert(pressure == 303.0, "pressure(301) + data(2) .eq. 303 = T")

    ! add arrayGridFull
    incTemp = 3.0
    pressure = pressure + incTemp
    call self % assert(pressure == 306.0, "pressure(303) + incTemp(3) .eq. 306 = T")


  end subroutine defineTestCases

end module shr_arrayGridFullRsp_test

