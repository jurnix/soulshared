!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : typeOptions_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> typeOptions_mod unit tests 
!>
!------------------------------------------------------------------------------
! change module name
module typeOptions_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: dp, sp
  use SHR_strings_mod, only: string
  use SHR_datetime_mod, only: datetime, timedelta
  use SHR_file_mod, only: file_t
  ! import here testing module
  use SHR_typeOptions_mod, only: option_abs, option_files, option_reals, &
                        option_ints, option_datetimes, option_timedeltas, &
                        option_str

  use SHR_typeOptions_mod, only: argsRange, OPT_NARGS_N

  implicit none

  private
  public :: testSuiteTypeOptions


  ! change here test suite name
  type, extends(testSuite) :: testSuiteTypeOptions

  contains
    procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteTypeOptions), intent(inout) :: self

    type(argsRange) :: ar

    type(option_str) :: ostr, ostrChoice
    type(option_files) :: ofiles, ofilesChoice
    type(option_reals) :: oreals, oRealsChoice
    type(option_reals) :: orealsDef
    type(option_ints) :: oints, ointsChoice
    type(option_datetimes) :: odates, odatesChoice
    type(option_timedeltas) :: otds, otdsChoice

    type(string) :: filenames(4)
    type(string) :: options(3)
    type(string) :: someStrs(2)
    type(string) :: expDefault

    type(string), allocatable :: foundStrs(:)
    type(string) :: outfileChoices(3)

    type(file_t), allocatable :: foundDefFile(:)
    type(file_t) :: expDefFile
    real(kind=sp), allocatable :: foundDefReal(:)
    integer, allocatable :: foundInt(:)
    type(datetime), allocatable :: foundDates(:)
    type(timedelta), allocatable :: foundTimedelta(:)

    type(string) :: failnc, somenc
    type(string), allocatable :: choiceValues(:)

    type(string) :: f1900nc, f1901nc, f1902nc
    type(datetime) :: dt1900, dt1901, dt1902
    type(timedelta) :: td30min, td1day

    real(kind=sp), allocatable ::  readRealValues(:)
    integer, allocatable ::  readIntValues(:)
    type(file_t), allocatable :: readFileValues(:)
    type(datetime), allocatable :: readDateValues(:)
    type(timedelta), allocatable :: readDeltaValues(:)

    expDefFile = file_t("1900.nc")

    expDefault = string("restout.nc")

    filenames(1) = "1901.nc"
    filenames(2) = "1902.nc"
    filenames(3) = "1903.nc"
    filenames(4) = "1904.nc"
    
    options(1) = "orchidee"
    options(2) = "pkulsm"
    options(3) = "other"

    someStrs(1) = "first"
    someStrs(2) = "second"

    outfileChoices(1) = "orchidee"
    outfileChoices(2) = "pkulsm"
    outfileChoices(3) = "wrf"

    ! constructors
    ostr = option_str("OUTPUTNAME", nargs = [2], description = "Output file name", default = [string("restout.nc")])
    ofiles = option_files("FORCINGS", nargs = [1], description = "Forcing files", default = [string("1900.nc")])
    oreals = option_reals("TEMP", nargs = [1, 3], description = "Temperature options", min=0., max=500.)
    oints = option_ints("LEVELS", nargs = [1], description = "Level options", min=1, max=10)
    odates = option_datetimes("STARTDATE", nargs = [1], description = "Simulation start time", default = [datetime(1900, 1, 1)])
    otds = option_timedeltas("TIMESTEP", nargs = [1], description = "Timestep length", default= [timedelta(days=1)])

    orealsDef = option_reals("TEMP", nargs = [1, 3], description = "Temperature options", &
            min=0., max=500., default = [273., 273., 273.])

    ! set values
    call ostr % getVal(foundStrs) ! default
    call self % assert (size(foundStrs) == 1, &
            "size of (option_str() % getVal ()) .eq. 1 == T")
    call self % assert (all(foundStrs == expDefault), &
            "option_str() % getVal () .eq. 'restout.nc' == T")


    call ofiles % getVal(foundDefFile) ! default
    call self % assert (all(foundDefFile == expDefFile), &
            "option_files()) % getVal () .eq. file_t('1900.nc') == T")

    call oreals % setVal([292.2_sp, 300.3_sp, 293.1_sp])
    call oreals % getVal(foundDefReal)
    call self % assert (all(foundDefReal == [292.2_sp, 300.3_sp, 293.1_sp]), &
            "option_reals()) % getVal () .eq. '292.2, ...' == T")

    call orealsDef % getVal(foundDefReal)
    call self % assert (all(foundDefReal == [273.0_sp, 273.0_sp, 273._sp]), &
            "option_reals()) % getVal () .eq. '273., ...' == T (default)")

    call oints % setVal(6)
    call oints % getVal(foundInt)
    call self % assert (all(foundInt == [6]), "option_ints()) % getVal () .eq. 6 == T")

    call odates % setVal(datetime(1910, 1, 1))
    call odates % getVal(foundDates)
    call self % assert (all(foundDates == datetime(1910, 1, 1)), &
            "option_datetimes()) % getVal () .eq. datetime(1910) == T")

    call otds % setVal(timedelta(days = 1))
    call otds % getVal(foundTimedelta)
    call self % assert (all(foundTimedelta == timedelta(days = 1)), &
            "option_timedeltas()) % getVal () .eq. timedelta(days=1) == T")


    ! re set values
    call ostr % setVal(someStrs)
    call ostr % getVal(foundStrs)
    call self % assert (all(foundStrs == someStrs), &
            "option_str(2) % getVal () .eq. [first, second] == T")

    call self % printBreakLine()

    ! argsRange
    ar = argsRange([1])
    call self % assert (ar % toString() == "1 allowed", "argsRange(1) .eq. '1 allowed' == T")
    call self % assert (ar % isAllowed(1), "argsRange(1) % isAllowed(1) == T")
    call self % assert (.not. ar % isAllowed(4), "argsRange(1) % isAllowed(4) == F")
    call self % assert (.not. ar % isAllowed(OPT_NARGS_N), "argsRange(1) % isAllowed(OPT_NARGS_N) == F")

    ar = argsRange([1, OPT_NARGS_N])
    call self % assert (ar % toString() == "1 to * allowed", "argsRange(1) .eq. '1 to * allowed' == T")
    call self % assert (.not. ar % isAllowed(0), "argsRange(1, unlimited) % isAllowed(0) == F")
    call self % assert (ar % isAllowed(20), "argsRange(1, unlimited) % isAllowed(20) == T")

    ar = argsRange([0, 5])
    call self % assert (ar % toString() == "0 to 5 allowed", "argsRange(0, unlimited) .eq. '0 to 5 allowed' == T")
    call self % assert (ar % isAllowed(0), "argsRange(0, unlimited) % isAllowed(0) == T")
    call self % assert (ar % isAllowed(5), "argsRange(0, unlimited) % isAllowed(5) == T")
    call self % assert (.not. ar % isAllowed(6), "argsRange(0, unlimited) % isAllowed(6) == F")
    
    ar = argsRange([3, 5])
    call self % assert (ar % getStartRange()  == 3, "argsRange(3, 5) % getStartRange() .eq. 3 == T")
    call self % assert (ar % getEndRange()  == 5, "argsRange(3, 5) % getEndRange() .eq. 5 == T")

    call self % printBreakLine()

    ! string choice
    failnc = string("fail.nc")
    somenc = string("some.nc")
    ostrChoice = option_str("OUTPUTNAME", nargs = [1], description = "Output file name", &
                                default = [string("restout.nc")], &
                                choices=[somenc, string("restout.nc"), failnc])
    call ostrChoice % setVal(failnc)
    call ostrChoice % getVal(choiceValues)
    call self % assert (choiceValues(1) == failnc, "option string choices .eq. fail.nc == T")

    ostrChoice = option_str("OUTPUTNAME", nargs = [2], description = "Output file name", &                                
                                choices=[somenc, string("restout.nc"), failnc])
    call ostrChoice % setVal([failnc, somenc])
    call ostrChoice % getVal(choiceValues)
    call self % assert (choiceValues(1) == failnc .and. choiceValues(2) == somenc &
                        , "option string choices .eq. (fail.nc, somenc) == T")

    ! file choice
    f1900nc = string("forcing1900.nc")
    f1901nc = string("forcing1901.nc")
    f1902nc = string("forcing1902.nc")
    ofilesChoice = option_files("FORCINGS", nargs = [1], description = "Forcing files", &
                default = [f1900nc], choices=[f1900nc, f1901nc, f1902nc])
    call ofilesChoice % setVal(f1902nc)
    call ofilesChoice % getVal(readFileValues)
    call self % assert (readFileValues(1) % getName() == f1902nc, "option file choices .eq. (1902.nc) == T")

    ! real choice
    orealsChoice = option_reals("MODE", nargs = [1], description = "Valid constant", choices=[1.1,22.2,3.3])
    call orealsChoice % setVal(3.3)
    call orealsChoice % getVal(readRealValues)
    call self % assert (readRealValues(1) == 3.3, "option real choices .eq. (3.3) == T")

    ! int choice
    ointsChoice = option_ints("LEVELS", nargs = [1], description = "Level options", choices=[1,2,3])
    call ointsChoice % setVal(2)
    call ointsChoice % getVal(readIntValues)
    call self % assert (readIntValues(1) == 2, "option int choices .eq. (2) == T")

    ! datetime choice
    dt1900 = datetime(1900,1,1)
    dt1901 = datetime(1901,1,1)
    dt1902 = datetime(1902,1,1)
    odatesChoice = option_datetimes("STARTDATE", nargs = [1], description = "Simulation start time", &
                choices=[dt1900, dt1901, dt1902])
    call odatesChoice % setVal(dt1900)
    call odatesChoice % getVal(readDateValues)
    call self % assert (readDateValues(1) == dt1900, "option datetime choices .eq. (1900,1,1) == T")

    ! timedelta choice
    td30min = timedelta(minutes=30)
    td1day = timedelta(hours=24)
    otdsChoice = option_timedeltas("TIMESTEP", nargs = [1], description = "Timestep length", &
                choices=[td30min, td1day])
    call otdschoice % setVal(td1day)
    call otdsChoice % getVal(readDeltaValues)
    call self % assert (readDeltaValues(1) == td1day, "option timedelta choices .eq. (1 day) == T")

  end subroutine defineTestCases


end module typeOptions_test

