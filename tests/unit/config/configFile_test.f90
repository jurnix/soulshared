!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : configFile_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> configFie_mod unit tests 
!------------------------------------------------------------------------------
!
module configFile_test
  use SHR_strings_mod, only: string
  use SHR_testSuite_mod, only: testSuite
  use SHR_configFile_mod, only: configFile

  implicit none

  private
  public :: testSuiteConfigFile

  type, extends(testSuite) :: testSuiteConfigFile
    contains
      procedure :: setUp
      procedure :: tearDown
      procedure :: define => defineTestCases
  end type 

contains

  subroutine setUp(self)
    class(testSuiteConfigFile), intent(inout) :: self
    integer :: stat

    open(unit=1234, iostat=stat, file='pkulsm.cfg', status='old')
    if (stat == 0) close(1234, status='delete')

    open(1, file = 'pkulsm.cfg', status = 'new')  
    write(1,*) "[pkulsm] # main section" 
    write(1,*) "pku_restart_from=restart1900.nc # restart simulation from this file" 
    write(1,*) "pku_forcings = 1900.nc, 1901.nc, 1902.nc"
    write(1,*) "pku_restart_output_name=restartsYYYY.nc" 
    write(1,*) "[solar radiation]" 
    write(1,*) "rad_enable=y  # always enabled" 
    write(1,*) "" 
    write(1,*) "   rad_c4=2.0, 3.0, 4.0, 5.0, &"
    write(1,*) "    5.5 &" 
    write(1,*) "    ,8.5, &" 
    write(1,*) "    0.5 " 
    write(1,*) "rad_c3=2, 3, 4, 5, 5" 
    write(1,*) "[vegetation]" 
    write(1,*) "slope=5"
    write(1,*) " #slope=5"
    write(1,*) "volume=5.2"
    write(1,*) "fixed=1,2,3,4&"
    write(1,*) "     ,5,6,7"

    close(1)

  end subroutine setUp


  subroutine tearDown(self)
    class(testSuiteConfigFile), intent(inout) :: self
    integer :: stat

    ! delete test config file if exists
    open(unit=1234, iostat=stat, file='pkulsm.cfg', status='old')
    if (stat == 0) close(1234, status='delete')
  end subroutine tearDown


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteConfigFile), intent(inout) :: self
    type(configFile) :: c
    type(string), allocatable :: forcings(:)
    real, allocatable :: radc4(:)

    character(len=:), allocatable :: intext, expected, found, restfile
    integer :: slope
    real :: increase
    integer :: cropscycle

    call self % setUp()

    call c % init("pkulsm.cfg")

    ! readAllLines

    ! addSection
    call c % addSection("test")
    call self % assert (.true., "c % addSection(test) == T")

    ! addOption
    call c % addOption("test", "vegetation", "the biggest")
    call self % assert (.true., "c % addOption(test, vegetation, the biggest) == T")

    call c % addOption("newtest", "vegetation", "the biggest")
    call self % assert (.true., "c % addOption(newtest, vegetation, the biggest) == T")

    call c % addOption("spitfire", "increase", 4.2)
    call c % addOption("crops", "cycle", 12)

    ! existsSection
    call self % assert (.not. c % existsSection("nothing"), "c % existsSection(nothing) == F")
    call self % assert (c % existsSection("test"), "c % existsSection(test) == T")
    call self % assert (c % existsSection("newtest"), "c % existsSection(newtest) == T")

    ! getOption
    call c % getOption_ss("pkulsm", "pku_restart_from", restfile)
    call self % assert (restfile .eq. "restart1900.nc", "c % getOption('pkulsm','pku_restart_from') .eq. restart1900.nc == T")

    call c % getOption_sr("pkulsm", "pku_forcings", forcings)
    call self % assert (forcings(2) .eq. "1901.nc", "c % getOption('pkulsm','pku_forcings')(2) .eq. 1901.nc == T")

    call c % getOption_rr("solar radiation", "rad_c4", radc4)
    call self % assert (radc4(3) .eq. 4, "c % getOption('solar radiation','rad_c4')(3) .eq. 3 == T")

    call c % getOption_is("vegetation", "slope", slope)
    call self % assert (slope .eq. 5, "c % getOption('vegetation','slope') .eq. 5 == T")
    
    call c % getOption_is("crops", "cycle", cropscycle)
    call self % assert (cropscycle .eq. 12, "c % getOption('crops','cycle') .eq. 12 == T")
    call c % getOption_rs("spitfire", "increase", increase)
    call self % assert (increase .eq. 4.2, "c % getOption('spitfire','increase') .eq. 4.2 == T")

    call self % tearDown()

  end subroutine defineTestCases

end module configFile_test

