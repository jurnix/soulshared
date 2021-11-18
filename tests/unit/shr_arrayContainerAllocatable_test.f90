!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : shr_arrayContainerAllocatable_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> common array subroutines 
!------------------------------------------------------------------------------
module shr_arrayContainerAllocatable_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp, dp

  use shr_arrayDim_mod, only: shr_arrayRspDim, shr_arrayDimContainer
  use shr_arrayContainer_mod, only: shr_arrayContainer
  use SHR_arrayContainerAllocatable_mod, only: shr_arrayContainerRspAllocatable

  implicit none

  private
  public :: testSuiteArrayContainerAllocatable


  type, extends(testSuite) :: testSuiteArrayContainerAllocatable
    contains
      procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArrayContainerAllocatable), intent(inout) :: self

    type(shr_arrayDimContainer), allocatable :: dims(:)
    class(shr_arrayRspDim), allocatable :: lat, lon
    class(shr_arrayContainerRspAllocatable), allocatable :: temperature, incTemp

    class(shr_arrayContainer), allocatable  :: genTemp, genIncTemp
    real(kind=sp) :: rawTemp(3,2)

    allocate(lat, lon)
    call lat % init("latitude", 1., 3., 1.)
    call lon % init("lontigude", 1., 2., 1.)

    allocate(dims(2))
    allocate(shr_arrayRspDim :: dims(1) % arrayDim)
    allocate(shr_arrayRspDim :: dims(2) % arrayDim)
    dims(1) % arrayDim = lat
    dims(2) % arrayDim = lon

    allocate(temperature, incTemp)
    call temperature % init(dims)
    call incTemp % init(dims)

    temperature = 300.
    call self % assert(temperature == 300., "temperature .eq. 300 = T")
    incTemp = 2.
    call self % assert(incTemp == 2., "incTemp .eq. 2 = T")

    ! operation (+)
    temperature = temperature + 1.0
    rawTemp = temperature  % r2
!    write(*,*) "shr_arrayContainerAllocatable_test:: rawTemp =", rawTemp
    call self % assert(temperature == 301., "temperature(300) + 1 .eq. 301 = T")

    rawTemp = 2.0
    temperature = temperature + rawTemp
    call self % assert(temperature == 303., "temperature(301) + 2 .eq. 303 = T")

    temperature = temperature + incTemp
    rawTemp = temperature % r2
!    write(*,*) "shr_arrayContainerAllocatable_test:: rawTemp =", rawTemp
    call self % assert(temperature == 305., "temperature(303) + 2 .eq. 305 = T")

    ! copy
    temperature = 300.
    incTemp = temperature
    call self % assert(incTemp == 300., "incTemp(300) .eq. 300. = T")

    rawTemp = 301.
    incTemp = rawTemp
    call self % assert(incTemp == 301., "incTemp(300) .eq. rawTemp(300) = T")

    incTemp = 0.1
    call self % assert(incTemp == 0.1, "incTemp(0.1) .eq. 0.1 = T")

    ! reverse copy
!    incTemp = 20.2
!    rawTemp = incTemp
!    call self % assert(rawTemp == 20.2, "(reverse copy) incTemp(20.2) .eq. 20.2 = T")

    ! equal
    temperature = 320.
    call self % assert(temperature == 320., "temperature(320) .eq. 320 = T")

    rawTemp = 325.
    temperature = rawTemp
    call self % assert(temperature == 325., "temperature(325) .eq. 325 = T")

    incTemp = 25.
    temperature = incTemp
    call self % assert(temperature == 25., "temperature(25.) .eq. 25. = T")

    ! generic
    temperature = 24.
    allocate(genTemp, source = temperature)
    allocate(genIncTemp, source = temperature)

    genTemp = genTemp
    select type (gt => genTemp)
    type is (shr_arrayContainerRspAllocatable)
      call self % assert(gt == 24., "genTemp(24.) .eq. 24. = T")
    class default
      call self % assert(.false., "genTemp(24.) .eq. 24. = T")
    end select


    ! genTemp = genTemp + genIncTemp
    select type (gt => genTemp)
    type is (shr_arrayContainerRspAllocatable)
      gT = gT + genIncTemp
      write(*,*) "shr_arrayContainerAllocatable_test:: genTemp= ", gT % r2
      call self % assert(gt == 48., "genTemp(48.) .eq. 48. = T")
    class default
      !< unexpected
      call self % assert(.false., "genTemp(48.) .eq. 48. = T")
    end select
     
  end subroutine defineTestCases

end module shr_arrayContainerAllocatable_test

