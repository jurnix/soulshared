!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        :  datetime_mod
!
!> @author
!> https://github.com/wavebitscientific/datetime-fortran
!
! DESCRIPTION:
!> Date and time manipulation for modern Fortran. A few functionalites have
!> been expanded:
!>   - clock
!>   - calendar types: gregorian, 360d and noleap
!------------------------------------------------------------------------------

module SHR_datetime_mod

  use iso_fortran_env, only: real32, real64
  use iso_c_binding, only: c_char, c_int, c_null_char
  use SHR_error_mod, only: raiseError

  implicit none

  private

  public :: datetime, timedelta, clock, calendar
  public :: date2num
  public :: datetimeRange
  public :: num2date
  public :: strptime
  public :: tm2date
  public :: tm_struct
  public :: c_strftime
  public :: c_strptime
  public :: DT_MAX_LEN
  public :: TD_MAX_LEN
  public :: CAL_DT_GREGORIAN, CAL_DT_NOLEAP, CAL_DT_360D, CAL_DT_NONE

  real(real64), parameter :: zero = 0._real64, one = 1._real64

  ! Constant multipliers to transform a number of some time unit to another
  real(real64), parameter :: d2h = 24._real64 ! day -> hour
  real(real64), parameter :: h2d = one / d2h ! hour -> day
  real(real64), parameter :: d2m = d2h * 60._real64 ! day -> minute
  real(real64), parameter :: m2d = one / d2m ! minute -> day
  real(real64), parameter :: m2h = one / 60 ! minute -> hour
  real(real64), parameter :: s2d = m2d / 60 ! second -> day
  real(real64), parameter :: d2s = 86400._real64 ! day -> second
  real(real64), parameter :: h2s = 3600._real64 ! hour -> second
  real(real64), parameter :: s2h = one / h2s ! second -> hour
  real(real64), parameter :: m2s = 60._real64 ! minute -> second
  real(real64), parameter :: s2m = one / m2s ! second -> minute
 
  integer, parameter :: MAXSTRLEN = 99 ! maximum string length for strftime

  integer, parameter :: MONTHS_YEAR = 12

  type :: calendar
     integer :: typeof 
     integer, dimension(MONTHS_YEAR) :: daysInMonth
     logical :: hasLeapYear
     integer :: daysInYear
     integer :: daysInLeapYear
  contains
     procedure, pass(self), public :: getType !< it returns the calendar type (integer)
     procedure, pass(self), public :: getDaysInMonth
     procedure, pass(self), public :: getDaysInYear
     procedure, pass(self), public :: isLeapYear
     procedure, pass(self), private :: calendar_assign

     generic :: assignment(=) => calendar_assign
  end type calendar

  ! Constant calendar types
  integer, parameter :: CAL_DT_NONE = -1 ! not defined
  integer, parameter :: CAL_DT_GREGORIAN = 1 ! modern calendar
  integer, parameter :: CAL_DT_NOLEAP = 2 ! no leap year (gregorian based) 365D
  integer, parameter :: CAL_DT_360D = 3 ! all months the same length (30 days)

  character(len=*), parameter :: CAL_NAMES(3) = [ "gregorian", "noleap   ", "360days  "]

  interface calendar
    module procedure :: calendar_constructor, calendar_constructor_str
  endinterface calendar

  integer, parameter :: days_gregorian(MONTHS_YEAR) = &
                [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  integer, parameter :: days_noleap(MONTHS_YEAR) = &
                [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  integer, parameter :: days_360(MONTHS_YEAR) = &
                [30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30]

  !< maximum output character length
  integer, parameter :: DT_MAX_LEN = 150


  type :: datetime

    private

    type(calendar) :: calendar_t ! calendar type [gregorian, no leap and 360d]
    integer :: year = 1 ! year [1-HUGE(year)]
    integer :: month = 1 ! month in year [1-12]
    integer :: day = 1 ! day in month [1-31]
    integer :: hour = 0 ! hour in day [0-23]
    integer :: minute = 0 ! minute in hour [0-59]
    integer :: second = 0 ! second in minute [0-59]
    integer :: millisecond = 0 ! milliseconds in second [0-999]
    real(real64) :: tz = 0 ! timezone offset from UTC [hours]

  contains

    ! getter functions
    procedure, pass(self), public :: getYear
    procedure, pass(self), public :: getMonth
    procedure, pass(self), public :: getDay
    procedure, pass(self), public :: getHour
    procedure, pass(self), public :: getMinute
    procedure, pass(self), public :: getSecond
    procedure, pass(self), public :: getMillisecond
    procedure, pass(self), public :: getTz
    procedure, pass(self), public :: getCalendarType

    ! public methods
    procedure, pass(self), public :: isocalendar
    procedure, pass(self), public :: isoformat
    procedure, pass(self), public :: isValid
    procedure, nopass,     public :: now
    procedure, pass(self), public :: secondsSinceEpoch
    procedure, pass(self), public :: strftime
    procedure, pass(self), public :: tm
    procedure, pass(self), public :: tzOffset
    procedure, pass(self), public :: isoweekday
    procedure, pass(self), public :: isoweekdayLong
    procedure, pass(self), public :: isoweekdayShort
    procedure, pass(self), public :: utc
    procedure, pass(self), public :: weekday
    procedure, pass(self), public :: weekdayLong
    procedure, pass(self), public :: weekdayShort
    procedure, pass(self), public :: yearday
    procedure, pass(self), public :: toString

    ! private methods
    procedure, pass(self), private :: addMilliseconds
    procedure, pass(self), private :: addSeconds
    procedure, pass(self), private :: addMinutes
    procedure, pass(self), private :: addHours
    procedure, pass(self), private :: addDays
    procedure, pass(self), private :: addMonths
    procedure, pass(self), private :: addYears

    ! operator overloading procedures
    procedure, pass(d0), private :: datetime_plus_timedelta
    procedure, pass(d0), private :: timedelta_plus_datetime
    procedure, pass(d0), private :: datetime_minus_datetime
    procedure, pass(d0), private :: datetime_minus_timedelta
    procedure, pass(d0), private :: datetime_eq
    procedure, pass(d0), private :: datetime_neq
    procedure, pass(d0), private :: datetime_gt
    procedure, pass(d0), private :: datetime_ge
    procedure, pass(d0), private :: datetime_lt
    procedure, pass(d0), private :: datetime_le
    procedure, pass(d0), private :: datetime_assign

    generic :: assignment(=) => datetime_assign
    generic :: operator(+)  => datetime_plus_timedelta, &
                               timedelta_plus_datetime
    generic :: operator(-)  => datetime_minus_datetime, &
                               datetime_minus_timedelta
    generic :: operator(==) => datetime_eq
    generic :: operator(/=) => datetime_neq
    generic :: operator(>)  => datetime_gt
    generic :: operator(>=) => datetime_ge
    generic :: operator(<)  => datetime_lt
    generic :: operator(<=) => datetime_le

  end type datetime

  interface datetime
    module procedure :: datetime_constructor
  endinterface datetime


  !< timedelta maximum length for chars
  integer, parameter :: TD_MAX_LEN = 100

  type :: timedelta
    private

    integer :: years = 0
    integer :: months = 0
    integer :: days = 0
    integer :: hours = 0
    integer :: minutes = 0
    integer :: seconds = 0
    integer :: milliseconds = 0

  contains

    procedure, pass(self), public :: getYears
    procedure, pass(self), public :: getMonths
    procedure, pass(self), public :: getDays
    procedure, pass(self), public :: getHours
    procedure, pass(self), public :: getMinutes
    procedure, pass(self), public :: getSeconds
    procedure, pass(self), public :: getMilliseconds
    procedure, pass(self), public :: toString => toString_timedelta

    procedure, public :: total_seconds

    procedure, private :: timedelta_mul_integer
    procedure, private :: timedelta_plus_timedelta
    procedure, private :: timedelta_minus_timedelta
    procedure, private :: timedelta_div_timedelta
    procedure, private :: unary_minus_timedelta
    procedure, private :: timedelta_eq
    procedure, private :: timedelta_neq
    procedure, private :: timedelta_gt
    procedure, private :: timedelta_ge
    procedure, private :: timedelta_lt
    procedure, private :: timedelta_le
    procedure, private :: timedelta_assign

    generic :: assignment(=) => timedelta_assign
    generic :: operator(*)  => timedelta_mul_integer
    generic :: operator(/)  => timedelta_div_timedelta
    generic :: operator(+)  => timedelta_plus_timedelta
    generic :: operator(-)  => timedelta_minus_timedelta, unary_minus_timedelta
    generic :: operator(==) => timedelta_eq
    generic :: operator(/=) => timedelta_neq
    generic :: operator(>)  => timedelta_gt
    generic :: operator(>=) => timedelta_ge
    generic :: operator(<)  => timedelta_lt
    generic :: operator(<=) => timedelta_le

  end type timedelta

  interface timedelta
    module procedure :: timedelta_constructor
  endinterface timedelta

  type,bind(c) :: tm_struct
    ! Derived type for compatibility with C and C++ struct tm.
    ! Enables calling strftime and strptime using iso_c_binding.
    ! See http://www.cplusplus.com/reference/ctime/tm for reference.
    integer(c_int) :: tm_sec = 0 ! Seconds [0-60] (1 leap second)
    integer(c_int) :: tm_min = 0 ! Minutes [0-59]
    integer(c_int) :: tm_hour = 0 ! Hours [0-23]
    integer(c_int) :: tm_mday = 0 ! Day [1-31]
    integer(c_int) :: tm_mon = 0 ! Month [0-11]
    integer(c_int) :: tm_year = 0 ! Year - 1900
    integer(c_int) :: tm_wday = 0 ! Day of week [0-6]
    integer(c_int) :: tm_yday = 0 ! Days in year [0-365]
    integer(c_int) :: tm_isdst = 0 ! DST [-1/0/1]
  end type tm_struct

  interface

    function c_strftime(str, slen, format, tm) bind(c, name='strftime') result(rc)
      ! Returns a formatted time string, given input time struct and format. 
      ! See https://www.cplusplus.com/reference/ctime/strftime for reference.
      import :: c_char, c_int, tm_struct
      character(kind=c_char), intent(out) :: str(*) ! result string
      integer(c_int), value, intent(in) :: slen ! string length
      character(kind=c_char), intent(in) :: format(*) ! time format 
      type(tm_struct), intent(in) :: tm ! tm_struct instance
      integer(c_int) :: rc ! return code
    end function c_strftime

    function c_strptime(str,format,tm) bind(c,name='strptime') result(rc)
      ! Interface to POSIX strptime.
      ! Returns a time struct object based on the input time string str and format.
      ! See http://man7.org/linux/man-pages/man3/strptime.3.html for reference.
      import :: c_char, c_int, tm_struct
      character(kind=c_char), intent(in) :: str(*) ! input string
      character(kind=c_char), intent(in) :: format(*) ! time format
      type(tm_struct), intent(out) :: tm ! result tm_struct
      integer(c_int) :: rc ! return code
    end function c_strptime

  end interface


  type :: clock
    type(datetime) :: startTime
    type(datetime) :: stopTime
    type(datetime) :: currentTime
    type(datetime) :: prevTime !> currentTime - 1 timestep
    type(datetime) :: nextTime !> currentTime + 1 timestep
    type(timedelta) :: tickInterval
    logical :: alarm = .false.
    logical :: started = .false.
    logical :: stopped = .false.
    logical :: nullified = .false. !< it is considered not defined
  contains
    procedure :: reset
    procedure :: tick
    procedure :: toString => toString_clock
    procedure :: isStopped 
    procedure :: isNull
    ! true  when the condition is satisfied for the current time step
    procedure :: isBeginYear, isEndYear  
    procedure :: isBeginMonth, isEndMonth
    procedure :: isBeginDay, isEndDay
    procedure :: isBeginClock, isEndClock
    procedure :: getTickIntervals
    procedure :: totalTickIntervals
    procedure :: currentTickIntervals
    !
    procedure, private :: clock_assign
    procedure, private, pass(from) :: clock_assign_tickInterval

    generic :: assignment(=) => clock_assign, clock_assign_tickInterval

    procedure :: findEquivalentCurrentTime
    procedure :: getCalendarType => getCalendarType_clock

    procedure, private :: equiv_clock
    generic :: operator(==) => equiv_clock
  end type clock

  interface clock
    module procedure :: clock_constructor
  endinterface clock

contains


  pure type(calendar) function calendar_constructor_str( &
    calendar_type)
    ! Constructor function for the `calendar` class.
    character(*), intent(in) :: calendar_type

    if (calendar_type == CAL_NAMES(CAL_DT_GREGORIAN)) then
      calendar_constructor_str = calendar_constructor(CAL_DT_GREGORIAN)
    else if (calendar_type == CAL_NAMES(CAL_DT_NOLEAP)) then
      calendar_constructor_str = calendar_constructor(CAL_DT_NOLEAP)
    else if (calendar_type == CAL_NAMES(CAL_DT_360D)) then
      calendar_constructor_str = calendar_constructor(CAL_DT_360D)
    else
      calendar_constructor_str = calendar_constructor(CAL_DT_GREGORIAN)
    endif

  end function calendar_constructor_str


  pure type(calendar) function calendar_constructor( &
    calendar_type)
    ! Constructor function for the `calendar` class.
    integer, intent(in), optional :: calendar_type
    integer :: calendar_to_use

    calendar_to_use = CAL_DT_GREGORIAN ! default calendar
    if (present(calendar_type)) calendar_to_use = calendar_type

    if (calendar_to_use .eq. CAL_DT_GREGORIAN) then
      calendar_constructor%daysInMonth = days_gregorian
      calendar_constructor%typeof = CAL_DT_GREGORIAN
      calendar_constructor%hasLeapYear = .true.
      calendar_constructor%daysInYear = 365 ! or 366 if leap year
      calendar_constructor%daysInLeapYear = 366

    else if (calendar_to_use .eq. CAL_DT_NOLEAP) then
      calendar_constructor%daysInMonth = days_noleap
      calendar_constructor%typeof = CAL_DT_NOLEAP
      calendar_constructor%hasLeapYear = .false.
      calendar_constructor%daysInYear = 365
      calendar_constructor%daysInLeapYear = -1 

    else if (calendar_to_use .eq. CAL_DT_360D) then
      calendar_constructor%daysInMonth = days_360
      calendar_constructor%typeof = CAL_DT_360D
      calendar_constructor%hasLeapYear = .false.
      calendar_constructor%daysInYear = 360
      calendar_constructor%daysInLeapYear = -1

    else  ! unexpected result 
      calendar_constructor%daysInMonth = [0,0,0,0,0,0,0,0,0,0,0,0]
      calendar_constructor%typeof = CAL_DT_NONE
      calendar_constructor%hasLeapYear = .false.
      calendar_constructor%daysInYear = 0
      calendar_constructor%daysInLeapYear = -1
    end if

  end function calendar_constructor


  pure subroutine calendar_assign(self, from)
    class(calendar), intent(inout) :: self
    class(calendar), intent(in) :: from

    self % typeof = from % typeof
    self % daysInMonth = from % daysInMonth
    self % hasLeapYear = from % hasLeapYear
    self % daysInYear = from % daysInYear
    self % daysInLeapYear = from % daysInLeapYear
  end subroutine calendar_assign


  pure subroutine clock_assign(self, from)
    class(clock), intent(inout) :: self
    class(clock), intent(in) :: from

    self % startTime = from % startTime
    self % stopTime = from % stopTime
    self % currentTime = from % currentTime
    self % prevTime = from % prevTime
    self % nextTime = from % nextTime
    self % tickInterval = from % tickInterval
    self % alarm = from % alarm
    self % started = from % started
    self % stopped = from % stopped
    self % nullified = from % nullified

  end subroutine clock_assign


  subroutine clock_assign_tickInterval(to, from)
    class(clock), intent(in) :: from
    type(timedelta), intent(inout) :: to

    to = from % tickInterval

  end subroutine clock_assign_tickInterval


  type(clock) function clock_constructor( &
          startTime, stopTime, timestep, startFromTime, caltype, isNull)
    !< Constructor function for the `clock` class.
    !< Calendar defined in startTime and stopTime must be the same
    !< In case caltype is define, it overrides startTime and stopTime calendars
    class(datetime), intent(in) :: startTime, stopTime
    class(timedelta), intent(in) :: timestep
    class(datetime), intent(in), optional :: startFromTime
    integer, intent(in), optional :: caltype !< define calendar type
    logical, intent(in), optional :: isNull

    type(calendar) :: inCalendar
    character(len=50) :: tmp, tmp1

    clock_constructor % nullified = .false.
    if (present(isNull)) then
        clock_constructor % nullified = isNull
        return
    endif

    if (present(caltype)) then
      inCalendar = calendar(caltype)
    else
      inCalendar = startTime % calendar_t
    endif

    clock_constructor % startTime = startTime
    clock_constructor % startTime % calendar_t = inCalendar 
    clock_constructor % stopTime = stopTime
    clock_constructor % stopTime % calendar_t = inCalendar
    clock_constructor % tickInterval = timestep
    if (present(startFromTime)) then
!      write(*,*) "datetime_mod:: clock_constructor:: currenttime is defined from startFromtime, ", startFromTime % toString()
      clock_constructor % currentTime = startFromTime
      clock_constructor % started = .true.
    else
!      write(*,*) "datetime_mod:: clock_constructor:: startTime is copied from start time, ", clock_constructor % startTime % toString()
      clock_constructor % currentTime = clock_constructor % startTime
!      write(*,*) "datetime_mod:: clock_constructor:: currenttime is copied from start time, ", clock_constructor % currentTime % toString()
!      write(*,*) "datetime_mod:: clock_constructor:: startTime is copied from start time, ", clock_constructor % startTime % toString()
      clock_constructor % started = .false.
    endif
    clock_constructor % prevTime = clock_constructor % currentTime - timestep
    clock_constructor % nextTime = clock_constructor % currentTime + timestep

    if (clock_constructor % currentTime % getCalendarType() /= &
        clock_constructor % startTime % getCalendarType()) then
        write(tmp, *) clock_constructor % currentTime % getCalendarType()
        write(tmp1, *) clock_constructor % startTime % getCalendarType()
        call raiseError(__FILE__, "clock_constructor", &
                "startFromTime has a different calendar", &
                "current time calendar: "//clock_constructor % currentTime % toString(), &
                "start time calendar: "//clock_constructor % startTime % toString() )
    endif
  end function clock_constructor


  elemental pure logical function equiv_clock(m0, m1)
    !< true if m0 and m1 have the same attributes
    class(clock), intent(in) :: m0
    class(clock), intent(in) :: m1

    equiv_clock = m0 % startTime == m1 % startTime .and. &
                  m0 % stopTime == m1 % stopTime .and. &
                  m0 % currentTime == m1 % currentTime .and. &
                  m0 % prevTime == m1 % prevTime .and. &
                  m0 % nextTime == m1 % nextTime .and. &
                  m0 % tickInterval  == m1 % tickInterval .and. &
                  (m0 % alarm .eqv. m1 % alarm) .and. &
                  (m0 % started .eqv. m1 % started)  .and. & 
                  (m0 % stopped .eqv. m1 % stopped) .and. &
                  (m0 % nullified .eqv. m1 % nullified)
  end function equiv_clock


  logical function isBeginYear(self)
    ! The current timestep is the first of the year
    ! It is calculated by TS-1 vs TS
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isBeginYear", &
                        "The clock is null")
    isBeginYear = self % currentTime % getYear() .ne. self % prevTime % getYear()
  end function isBeginYear


  logical function isEndYear(self)
    ! The current timestep is the last of the year
    ! It is calculated by TS vs TS+1
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isEndYear", &
                        "The clock is null")
    isEndYear = self % currentTime % getYear() .ne. self % nextTime % getYear()
  end function isEndYear


  logical function isBeginMonth(self)
    ! The current timestep is the first of the month
    ! It is calculated by TS-1 vs TS
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isBeginMonth", &
                        "The clock is null")
    isBeginMonth = self % currentTime % getMonth() .ne. self % prevTime % getMonth()
  end function isBeginMonth


  logical function isEndMonth(self)
    ! The current timestep is the last of the month
    ! It is calculated by TS vs TS+1
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isEndMonth", &
                        "The clock is null")
    isEndMonth = self % currentTime % getMonth() .ne. self % nextTime % getMonth()
  end function isEndMonth


  logical function isBeginDay(self)
    ! The current timestep is the first of the month
    ! It is calculated by TS-1 vs TS
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isBeginDay", &
                        "The clock is null")
    isBeginDay = self % currentTime % getDay() .ne. self % prevTime % getDay()
  end function isBeginDay


  logical function isEndDay(self)
    ! The current timestep is the last of the month
    ! It is calculated by TS vs TS+1
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isEndDay", &
                                "The clock is null")
    isEndDay = self % currentTime % getDay() .ne. self % nextTime % getDay()
  end function isEndDay


  logical function isBeginClock(self)
    ! The current timestep is the first of the month
    ! It is calculated by TS-1 vs TS
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isBeginClock", &
                        "The clock is null")
    isBeginClock = self % currentTime .eq. self % startTime
  end function isBeginClock


  logical function isEndClock(self)
    ! The current timestep is the last of the month
    ! It is calculated by TS vs TS+1
    class(clock), intent(in) :: self

    if (self % isNull()) call raiseError(__FILE__, "isEndClock", &
                        "The clock is null")
    isEndClock = self % currentTime .eq. self % stopTime
  end function isEndClock


  function getTickIntervals(self) result (timestep)
    !< it returns the time step
    class(clock), intent(in) :: self
    type(timedelta) :: timestep
    timestep = self % tickInterval
  end function getTickIntervals


  integer function currentTickIntervals(self) result (tsteps)
    ! Total number of ticks intervals from the start to the current time
    class(clock), intent(in) :: self

    type(datetime) :: counterTime

    if (self % isNull()) call raiseError(__FILE__, "currentTickIntervals", &
                              "The clock is null")
    counterTime = self % startTime

    tsteps = 1
    do while (counterTime < self % currentTime)
      tsteps = tsteps + 1
      counterTime = counterTime + self % tickInterval
    end do

  end function currentTickIntervals


  integer function totalTickIntervals(self)
    ! Total number of ticks intervals
    class(clock), intent(in) :: self

    type(datetime) :: currentTime

    if (self % isNull()) call raiseError(__FILE__, "totalTickIntervals", &
                              "The clock is null")
    currentTime = self % startTime

    totalTickIntervals = 0
    do while (currentTime < self % stopTime)
      totalTickIntervals = totalTickIntervals + 1

      currentTime = currentTime + self % tickInterval
    end do

  end function totalTickIntervals


  subroutine reset(self)
    ! Resets the clock to its start time.
    class(clock), intent(in out) :: self
    if (self % isNull()) call raiseError(__FILE__, "reset", &
                        "The clock is null")
    self % currentTime = self % startTime
    self % prevTime = self % currentTime - self % tickInterval
    self % nextTime = self % currentTime + self % tickInterval
    self % started = .false.
    self % stopped = .false.
  end subroutine reset


  subroutine tick(self)
    ! Increments the currentTime of the clock instance by one tickInterval.
    class(clock), intent(in out) :: self
    if (self % isNull()) call raiseError(__FILE__, "tick", &
                        "The clock is null")
    if (self % stopped) return
    if (.not. self % started) then
      self % started = .true.
      self % currentTime = self % startTime
    end if
    self % currentTime = self % currentTime + self % tickInterval
    self % prevTime = self % currentTime - self % tickInterval
    self % nextTime = self % currentTime + self % tickInterval
    if (self % currentTime >= self % stopTime) self % stopped = .true.
  end subroutine tick


  function toString_clock(self) result (str_clock)
    ! returns a string representation of the clock 
    ! startTime <- currentTime -> endTime
    class(clock), intent(in) :: self
    character(len=:), allocatable :: str_clock
    if (self % isNull()) call raiseError(__FILE__, "toString_clock", &
                        "The clock is null")

    str_clock = trim(adjustl(self % startTime % toString()))//" <- "// &
                trim(adjustl(self % currentTime % toString()))//" -> "// &
                trim(adjustl(self % stopTime % toString()))
  end function toString_clock


  logical function isStopped(self)
    ! true if the clock reach the last timestep
    class(clock), intent(in) :: self
    if (self % isNull()) call raiseError(__FILE__, "isStopped", &
                        "The clock is null")
    isStopped = self % stopped
  end function isStopped


  logical function isNull(self)
    ! true if the clock is considered null 
    class(clock), intent(in) :: self
    isNull = self % nullified
  end function isNull


  type(datetime) function findEquivalentCurrentTime(self, otherClock)
    !< Find equivalent current datetime for 'self' given another clock current time
    !<
    !< example:
    !< otherClock ts > self ts
    !< self (ts=8h)               current date: 1900-1-10 08:00    
    !< otherClock (ts=1d) output: match date: 1900-1-10
    !< 
    !< otherClock ts < self ts (it has to be 'multiple')
    !< self (ts=1d)               current date: 1900-1-10
    !< otherClock (ts=8h) output: match date: 1900-1-10 00:00
    !< 
    !< otherClock ts = self ts
    !< self (ts=1d)               current date: 1900-1-10 00:00
    !< otherClock (ts=8h) output: match date: 1900-1-10 00:00
    !<
    !< An error is raised if self % stopTime < otherClock % startTime
    !< An error is raised if otherClock % stopTime < self % startTime
    !< An error is raised if otherclock Timestep is not a multiple of self % timestep 
    !<                  or the other way around (ts1d vs ts2d => ok, ts1d vs ts7d => fail)
    !<                                                  24 / 7 = 3.xxx
    !< An error is raised if otherClock current datetime > self % stopTime
    class(clock), intent(in) :: self
    type(clock), intent(in) :: otherClock

    class(clock), allocatable :: reqClockDt
    type(datetime) :: dateToMatch
    type(datetime) :: iteDt
    character(len=50) :: tmp1, tmp

    if (self % getCalendarType() /= otherClock % getCalendarType()) then
      write(tmp, *) self % getCalendarType()
      write(tmp1, *) otherClock % getCalendarType()
      call raiseError(__FILE__, "findEquivalentCurrentTime", &
              "Different calendar types found", &
              "Current calendar type: "//trim(tmp), &
              "otherClock: "//trim(tmp1) )
    endif

    if (self % tickInterval == otherClock % tickInterval) then 
      findEquivalentCurrentTime = otherClock % currentTime

    else ! self % tickInterval /= otherClock % tickInterval
      dateToMatch = otherClock % currentTime
      iteDt = self % startTime

!      write (*,*) "datetime_mod:: findEquivalentCurrentTime:: dateToMatch=", dateToMatch % toString()

      do while (iteDt < dateToMatch)    
!        write (*,*) "datetime_mod:: findEquivalentCurrentTime:: iteDt=", iteDt % toString() 
        iteDt = iteDt + self % tickInterval
      enddo
!      write (*,*) "datetime_mod:: findEquivalentCurrentTime:: iteDt=", iteDt % toString() 

      ! searching date is surpassed, let's move one step backwards
      if (iteDt > dateToMatch) then
        iteDt = iteDt - self % tickInterval       
      endif

      findEquivalentCurrentTime = iteDt
    endif
  end function findEquivalentCurrentTime


  integer function getCalendarType_clock(self)
    !< it returns the calendar type
    class(clock), intent(in) :: self

    getCalendarType_clock = self % startTime % getCalendarType()
  end function getCalendarType_clock


  pure elemental type(datetime) function datetime_constructor( &
    year, month, day, hour, minute, second, millisecond, tz, cal_type)
    ! Constructor function for the `datetime` class.
    integer, intent(in), optional :: year, month, day, hour, minute, second, millisecond
    integer, intent(in), optional :: cal_type
    real(real64), intent(in), optional :: tz ! timezone offset in hours

    integer :: cal_chosen

    datetime_constructor % year = 1
    if (present(year)) datetime_constructor % year = year

    datetime_constructor % month = 1
    if (present(month)) datetime_constructor % month = month

    datetime_constructor % day = 1
    if (present(day)) datetime_constructor % day = day

    datetime_constructor % hour = 0
    if (present(hour)) datetime_constructor % hour = hour

    datetime_constructor % minute = 0
    if (present(minute)) datetime_constructor % minute = minute

    datetime_constructor % second = 0
    if (present(second)) datetime_constructor % second = second

    datetime_constructor % millisecond = 0
    if (present(millisecond)) datetime_constructor % millisecond = millisecond

    datetime_constructor % tz = 0
    if (present(tz)) datetime_constructor % tz = tz

    cal_chosen = CAL_DT_GREGORIAN
    if (present(cal_type)) cal_chosen = cal_type
    datetime_constructor % calendar_t = calendar(cal_chosen)

  end function datetime_constructor


  pure elemental integer function getYear(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getYear = self % year
  end function getYear


  pure elemental integer function getMonth(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getMonth = self % month
  end function getMonth


  pure elemental integer function getDay(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getDay = self % day
  end function getDay


  pure elemental integer function getHour(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getHour = self % hour
  end function getHour


  pure elemental integer function getMinute(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getMinute = self % minute
  end function getMinute


  pure elemental integer function getSecond(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getSecond = self % second
  end function getSecond


  pure elemental integer function getMillisecond(self)
    ! Returns the year component
    class(datetime), intent(in) :: self
    getMillisecond = self % millisecond
  end function getMillisecond

  pure elemental real(real64) function getTz(self)
    ! Returns the timezone offset component
    class(datetime), intent(in) :: self
    getTz = self % tz
  end function getTz


  pure elemental integer function getCalendarType(self)
    ! Returns the timezone offset component
    class(datetime), intent(in) :: self

    getCalendarType = self % calendar_t % typeof 
  end function getCalendarType


  pure elemental subroutine addMilliseconds(self, ms)
    ! Adds an integer number of milliseconds to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: ms
    self % millisecond = self % millisecond + ms
    do
      if (self % millisecond >= 1000) then
        call self % addSeconds(self % millisecond / 1000)
        self % millisecond = mod(self % millisecond, 1000)
      else if (self % millisecond < 0) then
        call self % addSeconds(self % millisecond / 1000 - 1)
        self % millisecond = mod(self % millisecond, 1000) + 1000
      else
        exit
      end if
    end do
  end subroutine addMilliseconds


  pure elemental subroutine addSeconds(self, s)
    ! Adds an integer number of seconds to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: s
    self % second = self % second + s
    do
      if (self % second >= 60) then
        call self % addMinutes(self % second / 60)
        self % second = mod(self % second, 60)
      else if (self % second < 0) then
        call self % addMinutes(self % second / 60 - 1)
        self % second = mod(self % second, 60) + 60
      else
        exit
      end if
    end do
  end subroutine addSeconds


  pure elemental subroutine addMinutes(self,m)
    ! Adds an integer number of minutes to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: m
    self % minute = self % minute + m
    do
      if (self % minute >= 60) then
        call self % addHours(self % minute / 60)
        self % minute = mod(self % minute, 60)
      else if (self % minute < 0) then
        call self % addHours(self % minute / 60 - 1)
        self % minute = mod(self % minute, 60) + 60
      else
        exit
      end if
    end do
  end subroutine addMinutes


  pure elemental subroutine addHours(self,h)
    ! Adds an integer number of hours to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: h
    self % hour = self % hour + h
    do
      if (self % hour >= 24) then
        call self % addDays(self % hour / 24)
        self % hour = mod(self % hour, 24)
      else if (self % hour < 0) then
        call self % addDays(self % hour / 24 - 1)
        self % hour = mod(self % hour, 24) + 24
      else
        exit
      end if
    end do
  end subroutine addHours


  pure elemental subroutine addDays(self, d)
    ! Adds an integer number of dayss to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: d
    integer :: daysInCurrentMonth
    self % day = self % day + d
    do
      daysInCurrentMonth = self % calendar_t % getDaysInMonth(self % month, self % year)
      if (self % day > daysInCurrentMonth) then
        self % day = self % day - daysInCurrentMonth
        call self % addMonths(1)
      else if (self % day < 1) then
        call self % addMonths(-1)
        self % day = self % day + self % calendar_t % getDaysInMonth(self % month, self % year)
      else
        exit
      end if
    end do
  end subroutine addDays


  pure elemental subroutine addMonths(self, m)
    ! Adds an integer number of monthss to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: m
    self % month = self % month + m
    do
      if (self % month > 12) then
        call self % addYears(self % month / 12)
        self % month = mod(self % month, 12)
      else if (self % month < 1) then
        call self % addYears(self % month / 12 - 1)
        self % month = mod(self % month, 12) + 12
      else
        exit
      end if
    end do
  end subroutine addMonths


  pure elemental subroutine addYears(self, y)
    ! Adds an integer number of yearss to self. Called by `datetime`
    ! addition (`+`) and subtraction (`-`) operators.
    class(datetime), intent(in out) :: self
    integer, intent(in) :: y
    self % year = self % year + y
  end subroutine addYears


  pure elemental character(23) function isoformat(self,sep)
    ! Returns character string with time in ISO 8601 format.
    class(datetime), intent(in) :: self
    character, intent(in), optional :: sep
    character :: separator

    separator = 'T'
    if (present(sep)) separator = sep

    ! TODO below is a bit cumbersome and was implemented
    ! at a time before the interface to strftime. Now we
    ! could do something like:
    !
    ! isoformat = self % strftime('%Y-%m-%d'//separator//'%H:%M:%S')
    !
    isoformat = int2str(self % year,       4)//'-'//      &
                int2str(self % month,      2)//'-'//      &
                int2str(self % day,        2)//separator//&
                int2str(self % hour,       2)//':'//      &
                int2str(self % minute,     2)//':'//      &
                int2str(self % second,     2)//'.'//      &
                int2str(self % millisecond,3)

  end function isoformat


  pure elemental logical function isValid(self)
    ! Checks whether the `datetime` instance has valid component values.
    ! Returns `.true.` if the `datetime` instance is valid, and `.false.`
    ! otherwise.
    class(datetime), intent(in) :: self

    ! assume valid
    isValid = .true.

    if (self % year < 1) then
      isValid = .false.
      return
    end if

    if (self % month < 1 .or. self % month > 12) then
      isValid = .false.
      return
    end if

    if (self % day < 1 .or. &
       self % day > self % calendar_t % getDaysInMonth(self % month,self % year)) then
      isValid = .false.
      return
    end if

    if (self % hour < 0 .or. self % hour > 23) then
      isValid = .false.
      return
    end if

    if (self % minute < 0 .or. self % minute > 59) then
      isValid = .false.
      return
    end if

    if (self % second < 0 .or. self % second > 59) then
      isValid = .false.
      return
    end if

    if (self % millisecond < 0 .or. self % millisecond > 999) then
      isValid = .false.
      return
    end if

  end function isValid


  type(datetime) function now()
    ! Returns a `datetime` instance with current time.
    character(5) :: zone
    integer :: values(8)
    integer :: hour, minute

    ! Obtain local machine time zone information
    call date_and_time(zone=zone, values=values)

    read(zone(1:3), '(i3)') hour
    read(zone(4:5), '(i2)') minute

    now = datetime(year = values(1), month = values(2), day = values(3), &
                   hour = values(5), minute = values(6), second = values(7), &
                   millisecond = values(8))

    now % tz = hour + minute * m2h

  end function now


  pure elemental integer function weekday(self)
    ! Returns the day of the week calculated using Zeller's congruence.
    ! Returned value is an integer scalar in the range [0-6], such that:
    !
    ! 0: Sunday
    ! 1: Monday
    ! 2: Tuesday
    ! 3: Wednesday
    ! 4: Thursday
    ! 5: Friday
    ! 6: Saturday
    class(datetime), intent(in) :: self
    integer :: year, month, j, k

    year  = self % year
    month = self % month

    if (month <= 2) then
      month = month + 12
      year  = year - 1
    end if

    j = year / 100
    k = mod(year, 100)

    weekday = mod(self % day + ((month + 1) * 26) / 10 + k + k / 4 + j / 4 + 5 * j, 7) -1

    if (weekday < 0) weekday = 6

  end function weekday


  pure elemental integer function isoweekday(self)
    ! Returns the day of the week per ISO 8601 returned from weekday().
    ! Returned value is an integer scalar in the range [1-7].
    class(datetime), intent(in) :: self
    isoweekday = self % weekday()
    if (isoweekday == 0) isoweekday = 7
  end function isoweekday


  pure elemental character(9) function weekdayLong(self)
    ! Returns the full name of the day of the week.
    class(datetime), intent(in) :: self
    character(9), parameter :: &
      days(*) = ['Sunday   ', 'Monday   ', 'Tuesday  ','Wednesday', &
                 'Thursday ', 'Friday   ', 'Saturday ']
    weekdayLong = days(self % weekday() + 1)
  end function weekdayLong


  pure elemental character(9) function isoweekdayLong(self)
    ! Returns the full name of the day of the week for ISO 8601
    ! ordered weekdays.
    class(datetime), intent(in) :: self
    character(9), parameter :: &
      days(7) = ['Monday   ','Tuesday  ','Wednesday','Thursday ', &
                 'Friday   ','Saturday ','Sunday   ']
    isoweekdayLong = days(self % isoweekday())
  end function isoweekdayLong


  pure elemental character(3) function weekdayShort(self)
    ! Returns the short (3-letter) name of the day of the week.
    class(datetime), intent(in) :: self
    character(3), parameter :: days(7) = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
    weekdayShort = days(self % weekday() + 1)
  end function weekdayShort


  pure elemental character(3) function isoweekdayShort(self)
    ! Returns the short (3-letter) name of the day of the week
    ! based on ISO 8601 ordering.
    class(datetime), intent(in) :: self
    character(3), parameter :: days(7) = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun']
    isoweekdayShort = days(self % isoweekday())
  end function isoweekdayShort


  function isocalendar(self)
    ! Returns an array of 3 integers, year, week number, and week day,
    ! as defined by ISO 8601 week date. Essentially a wrapper around C
    ! `strftime` function.
    class(datetime), intent(in) :: self
    integer :: isocalendar(3)
    integer :: year, week, wday, rc
    character(20) :: string

    rc = c_strftime(string, len(string), '%G %V %u' // c_null_char, self % tm())

    read(string(1:4), '(i4)') year
    read(string(6:7), '(i2)') week
    read(string(9:9), '(i1)') wday

    isocalendar = [year, week, wday]

  end function isocalendar


  integer function secondsSinceEpoch(self)
    ! Returns an integer number of seconds since the UNIX Epoch,
    ! `1970-01-01 00:00:00`. Note that this is a wrapper around C's
    ! `strftime('%s')`, so the number of seconds will reflect the time
    ! zone of the local machine on which the function is being called.
    class(datetime), intent(in) :: self
    character(11) :: string
    string = self % strftime('%s')
    read(string, '(i10)') secondsSinceEpoch
  end function secondsSinceEpoch


  function strftime(self, format)
    ! Wrapper around C and C++ `strftime` function.
    class(datetime), intent(in) :: self
    character(*), intent(in)  :: format
    character(:), allocatable :: strftime
    integer :: n, rc
    character(MAXSTRLEN) :: resultString
    resultString = ""
    rc = c_strftime(resultString, MAXSTRLEN, trim(format) // c_null_char, &
                    self % tm())
    strftime = trim(resultString)
    n = len(strftime)
    strftime = strftime(1:n-1)
  end function strftime


  pure elemental type(tm_struct) function tm(self)
    ! Returns a `tm_struct` instance of the current `datetime`.
    class(datetime), intent(in) :: self
    tm % tm_sec = self % second
    tm % tm_min = self % minute
    tm % tm_hour = self % hour
    tm % tm_mday = self % day
    tm % tm_mon = self % month - 1
    tm % tm_year = self % year - 1900
    tm % tm_wday = self % weekday()
    tm % tm_yday = self % yearday() - 1
    tm % tm_isdst = -1
  end function tm


  pure elemental character(5) function tzOffset(self)
    ! Returns a character string with timezone offset in hours from UTC,
    ! in format +/-[hh][mm].
    class(datetime), intent(in) :: self
    integer :: hours,minutes
  
    if (self % tz < 0) then
      tzOffset(1:1) = '-'
    else
      tzOffset(1:1) = '+'
    end if

    hours = int(abs(self % tz))
    minutes = nint((abs(self % tz) - hours) * 60)

    if (minutes == 60) then
      minutes = 0
      hours = hours + 1
    end if

    write(tzOffset(2:5), '(2i2.2)') hours, minutes

  end function tzOffset


  pure elemental type(datetime) function utc(self)
    ! Returns the `datetime` instance at Coordinated Universal Time (UTC).
    class(datetime), intent(in) :: self
    integer :: hours, minutes, sgn
    hours = int(abs(self % tz))
    minutes = nint((abs(self % tz) - hours) * 60)
    sgn = int(sign(one, self % tz))
    utc = self - timedelta(hours=sgn * hours, minutes=sgn * minutes)
    utc % tz = 0
  end function utc


  pure elemental integer function yearday(self)
    ! Returns the integer day of the year (ordinal date).
    class(datetime), intent(in) :: self
    integer :: month
    yearday = 0
    do month = 1, self % month-1
      yearday = yearday + self % calendar_t % getDaysInMonth(month, self % year)
    end do
    yearday = yearday + self % day
  end function yearday


  impure elemental function toString(self) result(str_dt)
    ! Returns the string representation
    class(datetime), intent(in) :: self
!    character(len=:), allocatable :: str_dt
    character(len=150) :: str_dt

    character(len=50) :: year_str, ms_str
    character(len=2) :: mon_str, day_str, hour_str
    character(len=2) :: min_str, sec_str
    character(len=:), allocatable :: calname
    integer :: caltype

    write(year_str,'(I5.4)') self%getYear()
    write(mon_str,'(I0.2)') self%getMonth()
    write(day_str,'(I0.2)') self%getDay()
    write(hour_str,'(I0.2)') self%getHour()
    write(min_str,'(I0.2)') self%getMinute()
    write(sec_str,'(I0.2)') self%getSecond()
    write(ms_str,'(I5.4)') self%getMillisecond()

    caltype = self % getCalendarType()
    if (caltype == CAL_DT_GREGORIAN) then
      calname = "Gregorian"
    else if (caltype == CAL_DT_NOLEAP) then
      calname = "noleap"
    else if (caltype == CAL_DT_360D) then
      calname = "360days"
    else
      call raiseError(__FILE__, "toString", "Unexpected calendar type found")
    endif

    str_dt =trim(adjustl(year_str))//"-"//mon_str//"-"//day_str//" " &
            //hour_str//":"//min_str//":"//sec_str//" "//trim(adjustl(ms_str))//" " &
            //calname
  end function toString


  pure elemental function datetime_plus_timedelta(d0,t) result(d)
    ! Adds a `timedelta` instance to a `datetime` instance, and returns a
    ! new `datetime` instance. Overloads the operator `+`.
    class(datetime), intent(in) :: d0
    class(timedelta), intent(in) :: t
    type(datetime) :: d

    integer :: milliseconds, seconds, minutes, hours, days, months, years

    d = datetime(year = d0 % getYear(),               &
                 month = d0 % getMonth(),             &
                 day = d0 % getDay(),                 &
                 hour = d0 % getHour(),               &
                 minute = d0 % getMinute(),           &
                 second = d0 % getSecond(),           &
                 millisecond = d0 % getMillisecond(), &
                 tz = d0 % getTz())

    milliseconds = t % getMilliseconds()
    seconds = t % getSeconds()
    minutes = t % getMinutes()
    hours = t % getHours()
    days = t % getDays()
    months = t % getMonths()
    years = t % getYears()

    if (milliseconds /= 0) call d % addMilliseconds(milliseconds)
    if (seconds /= 0) call d % addSeconds(seconds)
    if (minutes /= 0) call d % addMinutes(minutes)
    if (hours /= 0) call d % addHours(hours)
    if (days /= 0) call d % addDays(days)
    if (months /= 0) call d % addMonths(months)
    if (years /= 0) call d % addYears(years)

  end function datetime_plus_timedelta


  pure elemental function timedelta_plus_datetime(t,d0) result(d)
    ! Adds a `timedelta` instance to a `datetime` instance, and returns a
    ! new `datetime` instance. Overloads the operator `+`.
    class(timedelta), intent(in) :: t
    class(datetime), intent(in) :: d0
    type(datetime) :: d
    d = d0 + t
  end function timedelta_plus_datetime


  pure elemental function datetime_minus_timedelta(d0,t) result(d)
    ! Subtracts a `timedelta` instance from a `datetime` instance and
    ! returns a new `datetime` instance. Overloads the operator `-`.
    class(datetime), intent(in) :: d0
    class(timedelta), intent(in) :: t
    type(datetime) :: d
    d = d0 + (-t)
  end function datetime_minus_timedelta


  pure elemental function datetime_minus_datetime(d0,d1) result(t)
    ! Subtracts a `datetime` instance from another `datetime` instance,
    ! and returns a `timedelta` instance. Overloads the operator `-`.
    class(datetime), intent(in) :: d0, d1
    type(timedelta) :: t
    real(real64) :: daysDiff
    integer :: days,hours,minutes,seconds,milliseconds
    integer :: sign_

    daysDiff = date2num(d0)-date2num(d1)

    if (daysDiff < 0) then
      sign_ = -1
      daysDiff = ABS(daysDiff)
    else
      sign_ = 1
    end if

    days         = int(daysDiff)
    hours        = int((daysDiff-days)*d2h)
    minutes      = int((daysDiff-days-hours*h2d)*d2m)
    seconds      = int((daysDiff-days-hours*h2d-minutes*m2d)*d2s)
    milliseconds = nint((daysDiff-days-hours*h2d-minutes*m2d&
                                 -seconds*s2d)*d2s*1e3_real64)

    t = timedelta(0,0,sign_*days,sign_*hours,sign_*minutes,sign_*seconds,&
                  sign_*milliseconds)

  end function datetime_minus_datetime


  pure elemental logical function datetime_gt(d0,d1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! greater than `d1` and `.false.` otherwise. Overloads the
    ! operator `>`.
    class(datetime), intent(in) :: d0, d1
    type(datetime) :: d0_utc, d1_utc

    ! Convert to UTC before making comparison
    d0_utc = d0 % utc()
    d1_utc = d1 % utc()

    ! Compare years
    if (d0_utc % year > d1_utc % year) then
      res = .true.
    else if (d0_utc % year < d1_utc % year) then
      res = .false.
    else

      ! Compare months
      if (d0_utc % month > d1_utc % month) then
        res = .true.
      else if (d0_utc % month < d1_utc % month) then
        res = .false.
      else

        ! Compare days
        if (d0_utc % day > d1_utc % day) then
          res = .true.
        else if (d0_utc % day < d1_utc % day) then
          res = .false.
        else

          ! Compare hours
          if (d0_utc % hour > d1_utc % hour) then
            res = .true.
          else if (d0_utc % hour < d1_utc % hour) then
            res = .false.
          else

            ! Compare minutes
            if (d0_utc % minute > d1_utc % minute) then
              res = .true.
            else if (d0_utc % minute < d1_utc % minute) then
              res = .false.
            else

              ! Compare seconds
              if (d0_utc % second > d1_utc % second) then
                res = .true.
              else if (d0_utc % second < d1_utc % second) then
                res = .false.
              else

                ! Compare milliseconds
                if (d0_utc % millisecond > d1_utc % millisecond) then
                  res = .true.
                else
                  res = .false.
                end if

              end if
            end if
          end if
        end if
      end if
    end if

  end function datetime_gt


  pure elemental logical function datetime_lt(d0,d1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! less than `d1` and `.false.` otherwise. Overloads the operator `<`.
    class(datetime), intent(in) :: d0, d1
    res = d1 > d0
  end function datetime_lt


  pure subroutine datetime_assign(d0, from)
    ! `datetime` assignment operator. 
    ! 
    class(datetime), intent(inout) :: d0
    class(datetime), intent(in) :: from

    d0 % calendar_t = from % calendar_t
    d0 % year = from % year
    d0 % month = from % month
    d0 % day = from % day
    d0 % hour = from % hour
    d0 % minute = from % minute
    d0 % second = from % second
    d0 % millisecond = from % millisecond
    d0 % tz = from % tz

  end subroutine datetime_assign


  pure elemental logical function datetime_eq(d0,d1) result(res)
    ! `datetime` comparison operator that returns `.true.` if `d0` is
    ! equal to `d1` and `.false.` otherwise. Overloads the operator `==`.
    class(datetime), intent(in) :: d0, d1
    type(datetime) :: d0_utc, d1_utc

    ! Convert to UTC before making comparison
    d0_utc = d0 % utc()
    d1_utc = d1 % utc()

    res = d0_utc % year        == d1_utc % year   .and. &
          d0_utc % month       == d1_utc % month  .and. &
          d0_utc % day         == d1_utc % day    .and. &
          d0_utc % hour        == d1_utc % hour   .and. &
          d0_utc % minute      == d1_utc % minute .and. &
          d0_utc % second      == d1_utc % second .and. &
          d0_utc % millisecond == d1_utc % millisecond

  end function datetime_eq


  pure elemental logical function datetime_neq(d0,d1) result(res)
    ! `datetime` comparison operator that eturns `.true.` if `d0` is
    ! not equal to `d1` and `.false.` otherwise. Overloads the operator `/=`.
    class(datetime), intent(in) :: d0, d1
    res = .not. d0 == d1
  end function datetime_neq


  pure elemental logical function datetime_ge(d0,d1) result(res)
    ! `datetime` comparison operator. Returns `.true.` if `d0` is greater
    ! than or equal to `d1` and `.false.` otherwise. Overloads the
    ! operator `>=`.
    class(datetime), intent(in) :: d0, d1
    res = d0 > d1 .or. d0 == d1
  end function datetime_ge


  pure elemental logical function datetime_le(d0,d1) result(res)
    ! `datetime` comparison operator. Returns `.true.` if `d0` is less
    ! than or equal to `d1`, and `.false.` otherwise. Overloads the
    ! operator `<=`.
    class(datetime), intent(in) :: d0, d1
    res = d1 > d0 .or. d0 == d1
  end function datetime_le


  pure elemental logical function isLeapYear(self,year)
    ! Returns `.true.` if year is leap year and `.false.` otherwise.
    class(calendar), intent(in) :: self
    integer, intent(in) :: year

    isLeapYear = .false.
    if (self % hasLeapYear) then
      isLeapYear = (mod(year,4) == 0 .and. .not. mod(year,100) == 0)&
            .or. (mod(year,400) == 0)
    end if
  end function isLeapYear


  pure function datetimeRange(d0, d1, t)
    ! Given start and end `datetime` instances `d0` and `d1` and time
    ! increment as `timedelta` instance `t`, returns an array of
    ! `datetime` instances. The number of elements is the number of whole
    ! time increments contained between datetimes `d0` and `d1`.
    type(datetime), intent(in) :: d0, d1
    type(timedelta), intent(in) :: t
    real(real64) :: datenum0, datenum1, eps, increment
    type(datetime), allocatable :: datetimeRange(:)
    integer :: n, nm
    eps = 1e-10_real64
    datenum0 = date2num(d0)
    datenum1 = date2num(d1)
    increment = t % total_seconds() * s2d
    nm = floor((datenum1 - datenum0 + eps) / increment) + 1
    allocate(datetimeRange(nm))
    do n = 1, nm
      datetimeRange(n) = num2date(datenum0 + (n - 1) * increment)
    end do
  end function datetimeRange


  pure elemental integer function getType(self)
    !< returns the calendar type code (CAL_DT_GREGORIAN, ...)
    class(calendar), intent(in) :: self
    getType = self % typeof
  end function getType


  pure elemental integer function getDaysInMonth(self,month,year)
    ! Given integer month and year, returns an integer number
    ! of days in that particular month.
    class(calendar), intent(in) :: self
    integer, intent(in) :: month, year

    if (month < 1 .or. month > 12) then
      ! Should raise an error and abort here, however we want to keep
      ! the pure and elemental attributes. Make sure this function is
      ! called with the month argument in range.
      getDaysInMonth = 0
      return
    end if

    if (month == 2 .and. self % isLeapYear(year)) then
      getDaysInMonth = 29
    else
      getDaysInMonth = self % daysInMonth(month) 
    end if

  end function getDaysInMonth


  pure elemental integer function getDaysInYear(self,year)
    ! Returns the number of days in year.
    class(calendar), intent(in) :: self
    integer, intent(in) :: year
    if (self % isLeapYear(year)) then
      getDaysInYear = self % daysInLeapYear
    else
      getDaysInYear = self % daysInYear
    end if
  end function getDaysInYear


  pure elemental real(real64) function date2num(d)
    ! Given a datetime instance d, returns number of days since
    ! `0001-01-01 00:00:00`, taking into account the timezone offset.
    type(datetime), intent(in) :: d
    type(datetime) :: d_utc
    integer :: year

    ! Convert to UTC first
    d_utc = d % utc()

    ! d_utc % year must be positive:
    if (d_utc % year < 1) then
      date2num = 0
      return
    end if

    date2num = 0
    do year = 1,d_utc % year-1
      date2num = date2num + d % calendar_t % getDaysInYear(year)
    end do

    date2num = date2num          &
             + d_utc % yearday() &
             + d_utc % hour*h2d  &
             + d_utc % minute*m2d&
             + (d_utc % second+1e-3_real64*d_utc % millisecond)*s2d

  end function date2num


  pure elemental type(datetime) function num2date(num, calendar_chosen)
    ! Given number of days since `0001-01-01 00:00:00`, returns a
    ! correspoding `datetime` instance.
    real(real64), intent(in) :: num
    integer, optional, intent(in) :: calendar_chosen
    integer :: year, month, day, hour, minute, second, millisecond
    real(real64) :: days, totseconds
    integer :: calendar_sel
    type(calendar) :: calendar_t

    ! num must be positive
    if (num < 0) then
      num2date = datetime(1)
      return
    end if

    days = num

    calendar_sel = CAL_DT_GREGORIAN
    if (present(calendar_chosen)) calendar_sel = calendar_chosen
    calendar_t = calendar(calendar_sel)
    year = 1
    do
      if (int(days) <= calendar_t % getDaysInYear(year))exit
      days = days-calendar_t % getDaysInYear(year)
      year = year+1
    end do

    month = 1
    do
      if (inT(days) <= calendar_t % getDaysInMonth(month,year))exit
      days = days-calendar_t % getDaysInMonth(month,year)
      month = month+1
    end do

    day         = int(days)
    totseconds  = (days-day)*d2s
    hour        = int(totseconds*s2h)
    minute      = int((totseconds-hour*h2s)*s2m)
    second      = int(totseconds-hour*h2s-minute*m2s)
    millisecond = nint((totseconds-int(totseconds))*1e3_real64)

    num2date = datetime(year,month,day,hour,minute,second,millisecond,tz=zero,cal_type=calendar_chosen)

    ! Handle a special case caused by floating-point arithmethic:
    if (num2date % millisecond == 1000) then
      num2date % millisecond = 0
      call num2date % addSeconds(1)
    end if

    if (num2date % second == 60) then
      num2date % second = 0
      call num2date % addMinutes(1)
    end if
    if (num2date % minute == 60) then
      num2date % minute = 0
      call num2date % addHours(1)
    end if
    if (num2date % hour == 24) then
      num2date % hour = 0
      call num2date % addDays(1)
    end if

  end function num2date


  type(datetime) function strptime(str,format)
    ! A wrapper function around C/C++ strptime function.
    ! Returns a `datetime` instance.
    character(*), intent(in) :: str, format
    integer :: rc
    type(tm_struct) :: tm
    rc = c_strptime(trim(str) // c_null_char, trim(format) // c_null_char, tm)
    strptime = tm2date(tm)
  end function strptime


  pure elemental type(datetime) function tm2date(ctime)
    ! Given a `tm_struct` instance, returns a corresponding `datetime`
    ! instance.
    type(tm_struct), intent(in) :: ctime

    tm2date % millisecond = 0
    tm2date % second      = ctime % tm_sec
    tm2date % minute      = ctime % tm_min
    tm2date % hour        = ctime % tm_hour
    tm2date % day         = ctime % tm_mday
    tm2date % month       = ctime % tm_mon+1
    tm2date % year        = ctime % tm_year+1900
    tm2date % calendar_t  = calendar()
    tm2date % tz          = 0

  end function tm2date


  pure function int2str(i, length)
    ! Converts an integer `i` into a character string of requested length,
    ! pre-pending zeros if necessary.
    integer, intent(in) :: i, length
    character(length) :: int2str
    character(2) :: string
    write(string, '(i2)') length
    write(int2str, '(i' // string // '.' // string //')') i
  end function int2str


  pure elemental type(timedelta) function timedelta_constructor( &
    years, months, days, hours, minutes, seconds, milliseconds)
    ! Constructor function for the `timedelta` class. 
    integer, intent(in), optional :: years, months, days, hours, minutes, seconds, milliseconds
  
    timedelta_constructor % years = 0
    if (present(years)) timedelta_constructor % years = years
  
    timedelta_constructor % months = 0
    if (present(months)) timedelta_constructor % months = months
  
    timedelta_constructor % days = 0
    if (present(days)) timedelta_constructor % days = days

    timedelta_constructor % hours = 0
    if (present(hours)) timedelta_constructor % hours = hours

    timedelta_constructor % minutes = 0
    if (present(minutes)) timedelta_constructor % minutes = minutes

    timedelta_constructor % seconds = 0
    if (present(seconds)) timedelta_constructor % seconds = seconds

    timedelta_constructor % milliseconds = 0
    if (present(milliseconds)) timedelta_constructor % milliseconds = milliseconds

  end function timedelta_constructor


  pure elemental integer function getYears(self)
    ! Returns the number of years.
    class(timedelta), intent(in) :: self
    getYears = self % years
  end function getYears


  pure elemental integer function getMonths(self)
    ! Returns the number of months.
    class(timedelta), intent(in) :: self
    getMonths = self % months
  end function getMonths


  pure elemental integer function getDays(self)
    ! Returns the number of days.
    class(timedelta), intent(in) :: self
    getDays = self % days
  end function getDays


  pure elemental integer function getHours(self)
    ! Returns the number of hours.
    class(timedelta), intent(in) :: self
    getHours = self % hours
  end function getHours


  pure elemental integer function getMinutes(self)
    ! Returns the number of minutes.
    class(timedelta), intent(in) :: self
    getMinutes = self % minutes
  end function getMinutes


  pure elemental integer function getSeconds(self)
    ! Returns the number of seconds.
    class(timedelta), intent(in) :: self
    getSeconds = self % seconds
  end function getSeconds


  pure elemental integer function getMilliseconds(self)
    ! Returns the number of milliseconds.
    class(timedelta), intent(in) :: self
    getMilliseconds = self % milliseconds
  end function getMilliseconds


  pure elemental function toString_timedelta(self) result (outRes)
    !< Returns a string representation of timedelta
    !< Example:
    !<     "10y 10M"
    !<     "5h 5m 5s"
    class(timedelta), intent(in) :: self
    character(len=TD_MAX_LEN) :: outRes !< output

    character(len=:), allocatable :: res 
    character(len=200) :: tmp

    character(len=:), allocatable :: tYear, tMon, tDay, tHour, tMin, tSec, tMs
    logical :: hasSpace !< true if an space must be placed in front of tXXX

    character(len=:), allocatable :: spMon, spDay, spHour, spMin, spSec, spMs

    hasSpace = .false. !< first value has no space at the beginning
    spMon = ""
    spDay = ""
    spHour = ""
    spMin = ""
    spSec = ""
    spMs = ""

    tYear = ""
    if (self % years > 0) then
      !write(*,*) "toString_timedelta:: hasDays=", self % days
      write(tmp, *) self % years
      tYear = trim(adjustl(tmp))//"y"

      hasSpace = .true.
    end if

    tMon = ""
    if (self % months > 0) then
      !write(*,*) "toString_timedelta:: hasDays=", self % days
      write(tmp, *) self % months
      tMon = trim(adjustl(tmp))//"M"

      if (hasSpace) spMon = " "
      hasSpace = .true.
    end if

    tDay = ""
    if (self % days > 0) then
      write(tmp, *) self % days
      tDay = trim(adjustl(tmp))//"d"

      if (hasSpace) spDay = " "
      hasSpace = .true.
    end if

    tHour = ""
    if (self % hours > 0) then
      write(tmp, *) self % hours
      tHour = trim(adjustl(tmp))//"h"

      if (hasSpace) spHour = " "
      hasSpace = .true.
    endif

    tMin = ""
    if (self % minutes > 0) then
      write(tmp, *) self % minutes
      tMin = trim(adjustl(tmp))//"m"

      if (hasSpace) spMin = " "
      hasSpace = .true.
    endif

    tSec = ""
    if (self % seconds > 0) then
      write(tmp, *) self % seconds
      tSec = trim(adjustl(tmp))//"s"

      if (hasSpace) spSec = " "
      hasSpace = .true.
    endif

    tMs = ""
    if (self % milliseconds > 0) then
      write(tmp, *) self % milliseconds 
      tMs = trim(adjustl(tmp))//"ms"

      if (hasSpace) spMs = " "
      hasSpace = .true.
    endif
    outRes = tYear // &
            spMon // tMon //&
            spDay // tDay // &
            spHour // tHour // &
            spMin // tMin // &
            spSec // tSec // &
            tMs 
    
  end function toString_timedelta


  pure elemental real(real64) function total_seconds(self)
    ! Returns a total number of seconds contained in a `timedelta` 
    ! instance.
    class(timedelta), intent(in) :: self
    total_seconds = self % years*86400*365._real64 & 
                  + self % months*86400*30._real64 & 
                  + self % days*86400._real64 & 
                  + self % hours*3600._real64 &
                  + self % minutes*60._real64 &
                  + self % seconds            &
                  + self % milliseconds*1e-3_real64
  end function total_seconds


  pure elemental real(real64) function timedelta_div_timedelta(t0,t1) result(t)
    ! Divides t0 / t1 `timedelta`s instances and returns t `timedelta` 
    ! instance. Overloads the operator `/`.
    class(timedelta), intent(in) :: t0
    class(timedelta), intent(in) :: t1
    t = t0 % total_seconds() / t1 % total_seconds()
  end function timedelta_div_timedelta


  pure elemental type(timedelta) function timedelta_mul_integer(t0,i1) result(t)
    ! Adds i1 `timedelta` instances together and returns a `timedelta` 
    ! instance. Overloads the operator `*`.
    class(timedelta), intent(in) :: t0
    integer, intent(in) :: i1
    t = timedelta(years        = t0 % years        * i1, &
                  months       = t0 % months       * i1, &
                  days         = t0 % days         * i1, &
                  hours        = t0 % hours        * i1, &
                  minutes      = t0 % minutes      * i1, &
                  seconds      = t0 % seconds      * i1, &
                  milliseconds = t0 % milliseconds * i1)
  end function timedelta_mul_integer


  pure elemental type(timedelta) function timedelta_plus_timedelta(t0,t1) result(t)
    ! Adds two `timedelta` instances together and returns a `timedelta` 
    ! instance. Overloads the operator `+`.
    class(timedelta), intent(in) :: t0, t1
    t = timedelta(years        = t0 % years        + t1 % years,   &
                  months       = t0 % months       + t1 % months,  &
                  days         = t0 % days         + t1 % days,    &
                  hours        = t0 % hours        + t1 % hours,   &
                  minutes      = t0 % minutes      + t1 % minutes, &
                  seconds      = t0 % seconds      + t1 % seconds, &
                  milliseconds = t0 % milliseconds + t1 % milliseconds)
  end function timedelta_plus_timedelta


  pure elemental type(timedelta) function timedelta_minus_timedelta(t0,t1) result(t)
    ! Subtracts a `timedelta` instance from another. Returns a 
    ! `timedelta` instance. Overloads the operator `-`.
    class(timedelta), intent(in) :: t0, t1
    t = t0 + (-t1)
  end function timedelta_minus_timedelta


  pure elemental type(timedelta) function unary_minus_timedelta(t0) result(t)
    ! Takes a negative of a `timedelta` instance. Overloads the operator `-`.
    class(timedelta), intent(in) :: t0
    t % years        = -t0 % years
    t % months       = -t0 % months
    t % days         = -t0 % days
    t % hours        = -t0 % hours
    t % minutes      = -t0 % minutes
    t % seconds      = -t0 % seconds
    t % milliseconds = -t0 % milliseconds
  end function unary_minus_timedelta


  pure elemental logical function timedelta_eq(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0` 
    ! is equal to `td1` and `.false.` otherwise. Overloads the operator 
    ! `==`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() == td1 % total_seconds()
  end function timedelta_eq


  pure elemental logical function timedelta_neq(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0` 
    ! is not equal to `td1` and `.false.` otherwise. Overloads the 
    ! operator `/=`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() /= td1 % total_seconds()
  end function timedelta_neq


  pure elemental logical function timedelta_gt(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if
    ! `td0` is greater than `td1` and `.false.` otherwise. Overloads the 
    ! operator `>`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() > td1 % total_seconds()
  end function timedelta_gt


  pure elemental logical function timedelta_ge(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0` 
    ! is greater than or equal to `td1` and `.false.` otherwise. 
    ! Overloads the operator >=.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() >= td1 % total_seconds()
  end function timedelta_ge


  pure elemental logical function timedelta_lt(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0` 
    ! is less than `td1` and `.false.` otherwise. Overloads the operator 
    ! `<`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() < td1 % total_seconds()
  end function timedelta_lt


  pure elemental logical function timedelta_le(td0,td1) result(res)
    ! `timedelta` object comparison operator. Returns `.true.` if `td0` 
    ! is less than or equal to `td1` and `.false.` otherwise. Overloads 
    ! the operator `<=`.
    class(timedelta), intent(in) :: td0, td1
    res = td0 % total_seconds() <= td1 % total_seconds()
  end function timedelta_le


  pure subroutine timedelta_assign(d0, from)
    ! `datetime` assignment operator. 
    ! 
    class(timedelta), intent(inout) :: d0
    class(timedelta), intent(in) :: from

    d0 % years = from % years
    d0 % months = from % months
    d0 % days = from % days
    d0 % hours = from % hours
    d0 % minutes = from % minutes
    d0 % seconds = from % seconds
    d0 % milliseconds = from % milliseconds

  end subroutine timedelta_assign

end module SHR_datetime_mod
