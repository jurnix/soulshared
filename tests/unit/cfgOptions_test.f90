!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : cfgOptions_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> cfgOptions_mod unit tests 
!>
!------------------------------------------------------------------------------
! change module name
module cfgOptions_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: dp, sp
  use SHR_file_mod, only: removeIfExists, file_t, FILE_NAME_MAX_LEN
  use SHR_strings_mod, only: string
  use SHR_datetime_mod, only: timedelta, datetime
  ! import here testing module
  use SHR_cfgOptions_mod, only: cfgOptions_t
  use SHR_cfgOptions_mod, only: CFG_STR, CFG_FILES, CFG_REALS, CFG_INTS, &
                            CFG_TIMEDELTAS, CFG_DATETIMES, CFG_BOOLS
  use SHR_cfgOptions_mod, only: CFG_OPT_ARGS_N

  implicit none

  private
  public :: testSuiteCfgOptions

  character(len=*), parameter :: FILECFG = "test.cfg" !< config test file name

  ! change here test suite name
  type, extends(testSuite) :: testSuiteCfgOptions

  contains
    procedure :: define => defineTestCases
    procedure :: setUp
    procedure :: tearDown
  end type 

contains

  subroutine setUp(self)
    !< 
    class(testSuiteCfgOptions), intent(inout) :: self

    call removeIfExists(FILECFG)

    open(1, file = FILECFG, status = 'new')  
    write(1,*) "[pkulsm] # main section" 
    write(1,*) "" 
    write(1,*) "pku_restart_from=restart1901.nc # restart simulation from this file" 
    write(1,*) "pku_forcings = 1900.nc, 1901.nc, 1902.nc"
    write(1,*) "pku_restart_output_name=restartsYYYY.nc" 
    write(1,*) "pku_time_step= 00:30:00"
    write(1,*) "pku_time_start= 1900/01/01 00:00:00"
    write(1,*) "pku_time_end= 1903/01/01 00:00:00"
    write(1,*) "[solar radiation]" 
    write(1,*) "rad_enable=y  " 
    write(1,*) "" 
    write(1,*) "   rad_c4=2.0, 3.0, 4.0, 5.0, &"
    write(1,*) "    5.5 &" 
    write(1,*) "    ,8.5, &" 
    write(1,*) "    0.5 " 
    write(1,*) "rad_c3=2, 3, 4, 5, 5" 
    write(1,*) "[vegetation]" 
    write(1,*) "veg_slope=5"
    write(1,*) " #slope=7"
    write(1,*) "veg_volume=5.2"
    write(1,*) "veg_fixed=1,2,3,4&"
    write(1,*) "     ,5,6,7"

    close(1)

    ! create config file
  end subroutine setUp 


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteCfgOptions), intent(inout) :: self

    type(cfgOptions_t) :: cfgOptions
    type(cfgOptions_t) :: cfgOptsDefAndLoad
    type(cfgOptions_t) :: cfgOpsNoFile
    type(file_t), allocatable :: restartFiles(:)
    type(file_t), allocatable :: forcingFiles(:)
    integer, allocatable :: vegfixed(:)
    type(string) :: filename
    type(string), allocatable :: forcingNames(:)
    character(len=FILE_NAME_MAX_LEN), allocatable :: forcingNamesChar(:)
    type(timedelta), allocatable :: timestep(:)
    type(datetime), allocatable :: startDt(:)
    real(kind=sp), allocatable :: c4_rad(:)
    logical, allocatable :: solarRadEnabled(:)
    character(:), allocatable :: tmp

    type(file_t) :: somefiles(1)
    type(file_t), allocatable :: readFiles(:)
    type(timedelta) :: modelTS(1)
    type(timedelta), allocatable :: readModelTS(:)
    type(datetime) :: modelStart(1)
    type(datetime), allocatable :: readModelStart(:)
    logical :: compEnabled(1)
    logical, allocatable :: readCompEnabled(:)
    real(kind=sp) :: zeroAbs(1)
    real(kind=sp), allocatable :: readZeroAbs(:)
    integer :: levels(1)
    integer, allocatable :: readLevels(:)

    call self % setUp()

    call cfgOptions % init()
    call cfgOptions % load(FILECFG)

    ! pkulsm component
    call cfgOptions % addComponent("pkulsm")
    call cfgOptions % def ("pkulsm", "pku_restart_from", "Restart file to start from", &
            CFG_FILES, nargs = [1], defValS = [ string("restartfrom.nc") ])
    call cfgOptions % def ("pkulsm", "pku_forcings", "Forcing files", &
            CFG_FILES, nargs = [1, CFG_OPT_ARGS_N])
    call cfgOptions % def ("pkulsm", "pku_restart_output_name", "Output restart name", &
            CFG_FILES, nargs = [1])
    call cfgOptions % def ("pkulsm", "pku_time_step", "Simulation time step", &
            CFG_TIMEDELTAS, nargs = [1])
    call cfgOptions % def ("pkulsm", "pku_time_start", "Simulation start time", &
            CFG_DATETIMES, nargs = [1])
    call cfgOptions % def ("pkulsm", "pku_time_end", "Simulation end time", &
            CFG_DATETIMES, nargs = [1])

    ! vegetation component

    call cfgOptions % addComponent("vegetation")
    call cfgOptions % def ("vegetation", "veg_slope", "Vegetation slope constant", &
            CFG_INTS, nargs = [1], defValI=[1])
    call cfgOptions % def ("vegetation", "veg_volume", "Vegetation volume constant", &
            CFG_REALS, nargs = [1], defValR=[3._sp])
    call cfgOptions % def ("vegetation", "veg_fixed", "Fixed constant value", &
            CFG_INTS, nargs = [7], defValI=[2, 2, 2, 4, 5, 5, 5])

    ! solar radiation component

    call cfgOptions % addComponent("solar radiation")
    call cfgOptions % def ("solar radiation", "rad_c4", "c4 vegetation param", &
            CFG_REALS, nargs = [7])
    call cfgOptions % def ("solar radiation", "rad_enable", "Component enable", &
            CFG_BOOLS, nargs = [1])


    ! tests

    call cfgOptions % get("pkulsm", "pku_restart_from", restartFiles)
    filename = restartFiles(1) % getName()
    call self % assert( filename == "restart1901.nc" , &
          "pkuOption % get(pku_restart_from) .eq. 'restart1901.nc' == T") 


    call cfgOptions % get("pkulsm", "pku_forcings", forcingFiles)
    forcingNamesChar = forcingFiles % getName()
    forcingNames = string(forcingNamesChar)
    call self % assert( all(forcingNames == ["1900.nc", "1901.nc", "1902.nc"]) , &
          "pkuOption % get(pku_forcings) .eq. '1900.nc 1901.nc 1902.nc' == T") 

        
    call cfgOptions % get("vegetation", "veg_fixed", vegfixed)
    call self % assert( all(vegfixed == [1,2,3,4,5,6,7]) , &
          "pkuOption % get(veg_fixed) .eq. 1,2,3,4,5,6,7 == T") 

        
    call cfgOptions % get("pkulsm", "pku_time_step", timestep)
    call self % assert( timestep(1) == timedelta(minutes=30) , &
          "pkuOption % get(pku_time_step) .eq. timedelta(min=30) == T") 


    call cfgOptions % get("pkulsm", "pku_time_start", startDt)
    call self % assert( startDt(1) == datetime(1900, 1, 1) , &
          "pkuOption % get(pku_time_start) .eq. datetime(1900, 1, 1) == T") 


    call cfgOptions % get("solar radiation", "rad_c4", c4_rad)
    call self % assert( all(c4_rad == [2., 3., 4., 5., 5.5, 8.5, 0.5 ]) , &
          "pkuOption % get(c4_rad) .eq. (2, 3, 4, 5, 5.5, 8.5, 0.5) == T") 


    call cfgOptions % get("solar radiation", "rad_enable", solarRadEnabled)
    call self % assert( solarRadEnabled(1) , &
          "pkuOption % get(rad_enable) == T") 


    call self % printBreakLine()


    call cfgOpsNoFile % init()
    call cfgOpsNoFile % addComponent("second")
    call cfgOpsNoFile % def ("second", "sec_restart_from", "Restart file to start from", &
            CFG_FILES, nargs = [1])
    call cfgOpsNoFile % def ("second", "sec_forcings", "Forcing files", &
            CFG_FILES, nargs = [1, CFG_OPT_ARGS_N])
    call cfgOpsNoFile % def ("second", "sec_restart_output_name", "Output restart name", &
            CFG_FILES, nargs = [1])
    call cfgOpsNoFile % def ("second", "sec_time_step", "Simulation time step", &
            CFG_TIMEDELTAS, nargs = [1])
    call cfgOpsNoFile % def ("second", "sec_time_start", "Simulation start time", &
            CFG_DATETIMES, nargs = [1])
    call cfgOpsNoFile % def ("second", "sec_time_end", "Simulation end time", &
            CFG_DATETIMES, nargs = [1])
    call cfgOpsNoFile % def ("second", "sec_enabled", "Component enabled", &
            CFG_BOOLS, nargs = [1])
    call cfgOpsNoFile % def ("second", "sec_zeroabs", "Zero absolute into C", &
            CFG_REALS, nargs = [1])
    call cfgOpsNoFile % def ("second", "sec_level", "log levels messages", &
            CFG_INTS, nargs = [1])

    somefiles(1) = file_t("restart.nc")
    call cfgOpsNoFile % set("second", "sec_restart_from", somefiles)
    call cfgOpsNoFile % get("second", "sec_restart_from", readFiles)
    call self % assert(all(readFiles == somefiles) , "cfgOptions() % get(..) .eq. restart.nc  == T") 

    modelTS(1) = timedelta(hours=24)
    call cfgOpsNoFile % set("second", "sec_time_step", modelTS)
    call cfgOpsNoFile % get("second", "sec_time_step", readModelTS)
    call self % assert(all(modelTs == readModelTs) , "cfgOptions() % get(..) .eq. timedelta(hours=24)  == T") 

    modelStart(1) = datetime(1900)
    call cfgOpsNoFile % set("second", "sec_time_start", modelStart)
    call cfgOpsNoFile % get("second", "sec_time_start", readModelStart)
    call self % assert(all(modelStart == readModelStart) , "cfgOptions() % get(..) .eq. datetime(1900)  == T") 


    compEnabled(1) = .true.
    call cfgOpsNoFile % set("second", "sec_enabled", compEnabled)
    call cfgOpsNoFile % get("second", "sec_enabled", readCompEnabled)
    call self % assert(all(compEnabled .eqv. readCompEnabled) , "cfgOptions() % get(..) .eq. true  == T") 

    zeroabs(1) = -273.15
    call cfgOpsNoFile % set("second", "sec_zeroabs", zeroAbs)
    call cfgOpsNoFile % get("second", "sec_zeroabs", readzeroAbs)
    call self % assert(all(zeroAbs == readZeroAbs) , "cfgOptions() % get(..) .eq. -273.15  == T") 

    levels(1) = 2
    call cfgOpsNoFile % set("second", "sec_level", levels)
    call cfgOpsNoFile % get("second", "sec_level", readLevels)
    call self % assert(all(levels == readLevels) , "cfgOptions() % get(..) .eq. 2 == T") 


    call self % printBreakLine()


    ! define data and then load cfg file
    call cfgOptsDefAndLoad % init()
    call cfgOptsDefAndLoad % addComponent("pkulsm")
    call cfgOptsDefAndLoad % def ("pkulsm", "pku_restart_from", "Restart file to start from", &
            CFG_FILES, nargs = [1], defValS = [ string("restartfrom.nc") ])
    call cfgOptsDefAndLoad % def ("pkulsm", "pku_forcings", "Forcing files", &
            CFG_FILES, nargs = [1, CFG_OPT_ARGS_N])
    call cfgOptsDefAndLoad % def ("pkulsm", "pku_restart_output_name", "Output restart name", &
            CFG_FILES, nargs = [1])
    call cfgOptsDefAndLoad % def ("pkulsm", "pku_time_step", "Simulation time step", &
            CFG_TIMEDELTAS, nargs = [1])
    call cfgOptsDefAndLoad % def ("pkulsm", "pku_time_start", "Simulation start time", &
            CFG_DATETIMES, nargs = [1])
    call cfgOptsDefAndLoad % def ("pkulsm", "pku_time_end", "Simulation end time", &
            CFG_DATETIMES, nargs = [1])
    ! vegetation component

    call cfgOptsDefAndLoad % addComponent("vegetation")
    call cfgOptsDefAndLoad % def ("vegetation", "veg_slope", "Vegetation slope constant", &
            CFG_INTS, nargs = [1], defValI=[1])
    call cfgOptsDefAndLoad % def ("vegetation", "veg_volume", "Vegetation volume constant", &
            CFG_REALS, nargs = [1], defValR=[3._sp])
    call cfgOptsDefAndLoad % def ("vegetation", "veg_fixed", "Fixed constant value", &
            CFG_INTS, nargs = [7], defValI=[2, 2, 2, 4, 5, 5, 5])

    ! solar radiation component

    call cfgOptsDefAndLoad % addComponent("solar radiation")
    call cfgOptsDefAndLoad % def ("solar radiation", "rad_c4", "c4 vegetation param", &
            CFG_REALS, nargs = [7])
    call cfgOptsDefAndLoad % def ("solar radiation", "rad_enable", "Component enable", &
            CFG_BOOLS, nargs = [1])
    call cfgOptsDefAndLoad % load(FILECFG)

    ! timestep
    call cfgOptsDefAndLoad % get("pkulsm", "pku_time_step", timestep)
    call self % assert( timestep(1) == timedelta(minutes=30) , &
          "option % get(pku_time_step) .eq. timedelta(min=30) == T") 

    ! datetime 
    call cfgOptsDefAndLoad % get("pkulsm", "pku_time_start", startDt)
    call self % assert( startDt(1) == datetime(1900, 1, 1) , &
          "option % get(pku_time_start) .eq. datetime(1900, 1, 1) == T") 

    ! real
    call cfgOptsDefAndLoad % get("solar radiation", "rad_c4", c4_rad)
    call self % assert( all(c4_rad == [2., 3., 4., 5., 5.5, 8.5, 0.5 ]) , &
          "option % get(c4_rad) .eq. (2, 3, 4, 5, 5.5, 8.5, 0.5) == T") 

    ! bool
    call cfgOptsDefAndLoad % get("solar radiation", "rad_enable", solarRadEnabled)
    call self % assert( solarRadEnabled(1) , &
          "option % get(rad_enable) == T") 

    ! files 
    call cfgOptsDefAndLoad % get("pkulsm", "pku_forcings", forcingFiles)
    forcingNamesChar = forcingFiles % getName()
    forcingNames = string(forcingNamesChar)
    call self % assert( all(forcingNames == ["1900.nc", "1901.nc", "1902.nc"]) , &
          "option % get(pku_forcings) .eq. '1900.nc 1901.nc 1902.nc' == T") 
        
    ! int
    call cfgOptsDefAndLoad % get("vegetation", "veg_fixed", vegfixed)
    call self % assert( all(vegfixed == [1,2,3,4,5,6,7]) , &
          "option % get(veg_fixed) .eq. 1,2,3,4,5,6,7 == T") 

    ! string 


    call self % tearDown()
  end subroutine defineTestCases


  subroutine tearDown(self)
    !<  
    class(testSuiteCfgOptions), intent(inout) :: self
    integer :: stat

    call removeIfExists('FILECFG')

    ! remove config file
  end subroutine tearDown


end module cfgOptions_test

