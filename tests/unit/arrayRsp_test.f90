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
  use SHR_array_mod, only: shr_arrayRsp, shr_array

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
    type(shr_arrayRsp) :: temperature2d, temperature2dCopy
    type(shr_arrayRsp) :: incTemp
    type(shr_arrayDimContainer) :: tempDims(1)
    type(shr_arrayDimContainer) :: gridDims(2)
    type(shr_arrayDimContainer), allocatable :: foundDims(:)

    class(shr_array), allocatable :: pres, incPres

    type(shr_arrayRsp) :: tempCopy
    real(kind=sp), allocatable :: tempValues(:)
    real(kind=sp) :: rawData(3,3)
    real(kind=sp) :: incTempRawData(10)

    class(shr_arrayRsp), allocatable :: pressure, tair

!    lat = shr_arrayDim("latitude", 1., 90., 1.)
!    lon = shr_arrayDim("longitude", 1., 180., 1.)
!    levels = shr_arrayRspDim("levels", 1., 10., 1.)
    allocate(levels)
    call levels % init("levels", 1., 10., 1.)

    allocate(shr_arrayRspDim :: tempDims(1) % arrayDim)
    tempDims(1) % arrayDim = levels
    call temperature % init("temperature", tempDims, &
           "kelvin", "Air temperature at 1-100 meters" )

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
    temperature2dCopy = temperature2d

    call self % assert ( temperature2dCopy == 274., &
                "temperature2dCopy .eq. 274. = T"  )

    !
    allocate(tair)
    call tair % init("tair", tempDims, &
           "kelvin", "Air temperature at 1-100 meters" )
    allocate(pressure)
    call pressure % init("pressure", tempDims, &
           "Q", "Air pressure at 1-100 meters" )
    tair = 300.
    pressure = tair
    call self % assert ( pressure == 300., &
                "pressure .eq. 300. = T"  )

    ! generic
    allocate(pres, source = pressure)
    allocate(incPres, source = pressure)

    ! set value
    select type(p => pres)
    type is (shr_arrayRsp)
      p = 20.
    class default
    end select

    ! set value
    select type(ip => incPres)
    type is (shr_arrayRsp)
      ip = 1.
    class default
    end select

    ! equal (to iself)
    pres = pres
    call self % assert ( pres == pres, "pres copy(=) pres = T"  )

    ! add (to itself)
    pres = pres + incPres !< error
    call self % assert ( pres == pres, "pres (+ incPres) .eq. pres = T"  )
  end subroutine defineTestCases

end module arrayRsp_test

