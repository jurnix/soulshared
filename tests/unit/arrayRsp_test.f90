!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : arrayRsp_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> common array subroutines 
!------------------------------------------------------------------------------
module arrayRsp_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp, dp

  use shr_strings_mod, only: string
  use shr_arrayDim_mod, only: shr_arrayDim, shr_arrayRspDim, shr_arrayDimContainer
  use SHR_array_mod, only: shr_arrayRsp

  implicit none

  private
  public :: testSuiteArrayRsp


  type, extends(testSuite) :: testSuiteArrayRsp
    contains
      procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArrayRsp), intent(inout) :: self

!    type(shr_arrayDim) :: lat, lon
    class(shr_arrayRspDim), allocatable :: levels
    class(shr_arrayRspDim), allocatable :: lat, lon 
    type(shr_arrayRsp) :: temperature
    type(shr_arrayRsp) :: temperature2d
    type(shr_arrayRsp) :: incTemp
    type(shr_arrayDimContainer) :: tempDims(1)
    type(shr_arrayDimContainer) :: gridDims(2)
    type(shr_arrayDimContainer), allocatable :: foundDims(:)

    type(shr_arrayRsp) :: tempCopy
    real(kind=sp), allocatable :: tempValues(:)
    real(kind=sp) :: rawData(3,3)
    real(kind=sp) :: incTempRawData(10)

!    lat = shr_arrayDim("latitude", 1., 90., 1.)
!    lon = shr_arrayDim("longitude", 1., 180., 1.)
!    levels = shr_arrayRspDim("levels", 1., 10., 1.)
    allocate(levels)
    call levels % init("levels", 1., 10., 1.)

!    temperature = shr_array("temperature", [levels], &
!           "kelvin", "Air temperature at 1-100 meters" )
    allocate(shr_arrayRspDim :: tempDims(1) % arrayDim)
    tempDims(1) % arrayDim = levels
    call temperature % init("temperature", tempDims, &
           "kelvin", "Air temperature at 1-100 meters" )

!    incTemp = shr_array("incTemp", [levels], &
!           "kelvin", "Increase of air temperature" )
    call incTemp % init("incTemp", tempDims, &

           "kelvin", "Increase of air temperature" )

    temperature = 273. !< in Kelvin
    incTemp = 1.

    call self % assert ( temperature % getName() == string("temperature"), &
                "t % getName() .eq. temperature = T"  )

    call self % assert ( temperature % getUnits() == string("kelvin"), &
                "t % getUnits() .eq. kelvin = T"  )

    call self % assert ( temperature % getDescription() == &
                string("Air temperature at 1-100 meters"), &
                "t % getDescription() .eq. 'Air temperature ...' = T"  )

    call self % assert ( temperature % getSize() == 1, &
                "t % getSize() .eq. 1 = T"  )

    foundDims = temperature % getDims()
    call self % assert ( all(foundDims == tempDims), &
                "t % getDims() .eq. [levels] = T"  )

    call self % assert ( .not. (temperature == incTemp), &
                "temperature .eq. inctemp = F"  )

    call self % assert ( temperature == 273., &
                "temperature .eq. 273 = T"  )

    call self % assert ( temperature == temperature, &
                "temperature .eq. temperature = T"  )

    tempCopy = temperature
    tempValues = temperature
    incTempRawData = 1.0
    temperature = temperature + incTempRawData
    call self % assert ( temperature == 274., &
                "temperature .eq. temperature = T"  )

    call self % assert ( tempCopy == 273., &
                "tempCopy .eq. 273. = T"  )

    call self % assert ( all(tempValues == 273.), &
                "tempValues .eq. 273. = T"  )

    tempValues = tempValues + 1
    call self % assert ( all(tempValues == 274.), &
                "tempValues .eq. 274. = T"  )

    tempValues = tempValues - 2.0_dp
    call self % assert ( all(tempValues == 272.), &
                "tempValues .eq. 272. = T"  )

    ! 2d
    allocate(lat, lon)
    call lat % init("latitude", 1., 3., 1.)
    call lon % init("longitude", 1., 3., 1.)

    allocate(shr_arrayRspDim :: gridDims(1) % arrayDim)
    allocate(shr_arrayRspDim :: gridDims(2) % arrayDim)
    gridDims(1) % arrayDim = lat
    gridDims(2) % arrayDim = lon

    rawData(:,:) = 1.0

    call temperature2d % init("temperature", gridDims, &
           "kelvin", "Air temperature at 1-100 meters" )
    temperature2d = 273.0
    temperature2d = temperature2d + rawData
    call self % assert ( temperature2d == 274., &
                "temperature2d .eq. 274. = T"  )
  end subroutine defineTestCases

end module arrayRsp_test

