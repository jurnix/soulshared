module datetime_test
  use SHR_datetime_mod
  use iso_fortran_env, only: real64
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteDatetime

  type, extends(testSuite) :: testSuiteDatetime
    contains
      procedure :: define => test_datetime
  end type 

contains


  subroutine test_datetime(self)
    use iso_c_binding
    class(testSuiteDatetime), intent(inout) :: self

    type(datetime) :: a, b
    type(timedelta) :: td, copytd
    type(clock) :: c, cp
    type(calendar) :: cal
    type(calendar) :: cpcal

    type(datetime), allocatable :: dtRange(:)
    type(timedelta) :: timeone
    type(timedelta) :: timediff

    real(real64) :: eps = tiny(1._real64)
    integer :: i, tzOffset
    type(clock) :: clock1day, clock2days 
    type(datetime) :: tmpdt


    ! Empty constructor
    call self % assert(datetime() == datetime(1, 1, 1), &
                      'empty datetime() constructor')
    

    ! Empty time initialization
    call self % assert(datetime(2014, 1, 1) == datetime(2014, 1, 1, 0, 0, 0, 0), &
                      'semi-empty datetime() constructor')
    

    ! Increment milliseconds
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(milliseconds= 100) &
                   == datetime(2014, 1, 1, 0, 0, 0, 100),                             &
                   'datetime + timedelta(milliseconds = 100)')
    

    ! Decrement milliseconds
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(milliseconds=-100) &
                   == datetime(2013, 12, 31, 23, 59, 59, 900),                        &
                   'datetime + timedelta(milliseconds = -100)')
    

    ! Increment seconds
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(seconds=1) &
                   == datetime(2014, 1, 1, 0, 0, 1, 0),                       &
                   'datetime + timedelta(seconds = 1)')
    

    ! Decrement seconds
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(seconds=-1) &
                   == datetime(2013, 12, 31, 23, 59, 59, 0),                   &
                   'datetime + timedelta(seconds = -1)')
    

    ! Increment minutes
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(minutes=1) &
                   == datetime(2014, 1, 1, 0, 1, 0, 0),                       &
                   'datetime + timedelta(minutes = 1)')
    

    ! Decrement minutes
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(minutes=-1) &
                   == datetime(2013, 12, 31, 23, 59, 0, 0),                    &
                   'datetime + timedelta(minutes = -1)')
    

    ! Increment hours
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(hours=1) &
                   == datetime(2014, 1, 1, 1, 0, 0, 0),                     &
                   'datetime + timedelta(hours = 1)')
    

    ! Decrement hours
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(hours=-1) &
                   == datetime(2013, 12, 31, 23, 0, 0, 0),                   &
                   'datetime + timedelta(hours = -1)')
    

    ! Increment days
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(days= 1) &
                   == datetime(2014, 1, 2, 0, 0, 0, 0),                     &
                   'datetime + timedelta(days = 1)')
    

    ! Decrement days
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2013, 12, 31, 0, 0, 0, 0),                   &
                   'datetime + timedelta(days = -1)')
    

    ! Increment months
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(months= 1) &
                   == datetime(2014, 2, 1, 0, 0, 0, 0),                     &
                   'datetime + timedelta(months = 1)')
    

    ! Decrement months
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(months=-1)&
                   == datetime(2013, 12, 1, 0, 0, 0, 0),                   &
                   'datetime + timedelta(months = -1)')
    

    ! Increment years
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(years= 1) &
                   == datetime(2015, 1, 1, 0, 0, 0, 0),                     &
                   'datetime + timedelta(years = 1)')
    

    ! Decrement years
    call self % assert(datetime(2014, 1, 1, 0, 0, 0, 0) + timedelta(years=-1)&
                   == datetime(2013, 1, 1, 0, 0, 0, 0),                   &
                   'datetime + timedelta(years = -1)')

    ! 
    call self % assert(datetime(1900, 1, 1) - datetime(1900, 1, 1)&
                   == timedelta(0, 0, 0),                   &
                   'datetime - datetime = timedelta')

    

    ! Test various overflow situations

    a = datetime(2014, 1, 1, 0, 0, 0)

    call self % assert(a+timedelta(seconds=3) == a+timedelta(milliseconds=3000), &
                      'Seconds overflow in addMilliseconds (3000 milliseconds)')
    

    call self % assert(a-timedelta(seconds=3) == a-timedelta(milliseconds=3000), &
                      'Seconds overflow in addMilliseconds (-3000 milliseconds)')
    

    call self % assert(a+timedelta(minutes=6) == a+timedelta(seconds=360), &
                      'Minutes overflow in addSeconds (360 seconds)')
    

    call self % assert(a-timedelta(minutes=6) == a-timedelta(seconds=360), &
                      'Minutes overflow in addSeconds (-360 seconds)')
    

    call self % assert(a+timedelta(hours=6) == a+timedelta(minutes=360), &
                      'Hours overflow in addMinutes (360 minutes)')
    

    call self % assert(a-timedelta(hours=6) == a-timedelta(minutes=360), &
                      'Hours overflow in addMinutes (-360 minutes)')
    

    call self % assert(a+timedelta(days=3) == a+timedelta(hours=72), &
                      'Days overflow in addHours (72 hours)')
    

    call self % assert(a-timedelta(days=3) == a-timedelta(hours=72), &
                      'Days overflow in addHours (-72 hours)')
    

    call self % printBreakLine()

    ! Test subtracting into previous months:
    call self % assert(datetime(2014, 2,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 1, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into January')
    

    call self % assert(datetime(2014, 3,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 2, 28, 0, 0, 0, 0),                     &
                   'decrement datetime into February')
    

    call self % assert(datetime(2014, 4,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 3, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into March')
    

    call self % assert(datetime(2014, 5,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 4, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into April')
    

    call self % assert(datetime(2014, 6,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 5, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into May')
    

    call self % assert(datetime(2014, 7,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                 == datetime(2014, 6, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into June')
    

    call self % assert(datetime(2014, 8,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 7, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into July')
    

    call self % assert(datetime(2014, 9,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 8, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into August')
    

    call self % assert(datetime(2014, 10,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014,  9, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into September')
    

    call self % assert(datetime(2014, 11,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 10, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into October')
    

    call self % assert(datetime(2014, 12,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 11, 30, 0, 0, 0, 0),                     &
                   'decrement datetime into November')
    

    call self % assert(datetime(2015,  1,  1, 0, 0, 0, 0) + timedelta(days=-1)&
                   == datetime(2014, 12, 31, 0, 0, 0, 0),                     &
                   'decrement datetime into December')
    

    call self % printBreakLine()

    ! datetime difference
    timediff = datetime(2014, 1, 2)-datetime(2014, 1, 1)
    timeone =  timedelta(days=1)
    call self % assert(datetime(2014, 1, 2)-datetime(2014, 1, 1)&
                       == timedelta(days=1), &
                      'datetime-datetime == timedelta(days = 1)')
    

    call self % assert(datetime(2014, 1, 1, 2)-datetime(2014, 1, 1, 1)&
                       == timedelta(hours=1),                   &
                      'datetime-datetime == timedelta(hours = 1)')
    

    call self % assert(datetime(2014, 1, 1, 1, 2)-datetime(2014, 1, 1, 1, 1)&
                      == timedelta(minutes=1),                      &
                      'datetime-datetime == timedelta(minutes = 1)')
    

    call self % assert(datetime(2014, 1, 1, 1, 1, 2)-datetime(2014, 1, 1, 1, 1, 1)&
                      == timedelta(seconds=1),                          &
                      'datetime-datetime == timedelta(seconds = 1)')
    

    call self % assert(datetime(2014, 1, 1, 1, 1, 1, 2)&
                     -datetime(2014, 1, 1, 1, 1, 1, 1)&
                   == timedelta(milliseconds=1), &
                   'datetime-datetime == timedelta(milliseconds = 1)')
    

    call self % printBreakLine()

    ! datetime comparison tests
    call self % assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   == datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime == datetime')
    

    call self % assert(datetime(2014, 1, 2, 9, 4, 5, 6, tz=6._real64) &
                   == datetime(2014, 1, 2, 3, 4, 5, 6, tz=0._real64), &
                   'datetime == datetime,  timezone test 1')
    

    call self % assert(datetime(2014, 1, 2, 3, 4, 5, 6, tz=-6._real64) &
                   == datetime(2014, 1, 2, 9, 4, 5, 6, tz= 0._real64), &
                   'datetime == datetime,  timezone test 2')
    

    call self % assert(datetime(2013, 1, 2, 3, 4, 5, 6) &
                   /= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime /= datetime')
    

    call self % assert(datetime(2014, 1, 2, 4, 4, 5, 6) &
                    > datetime(2014, 1, 2, 3, 4, 5, 6), &
                    'datetime > datetime')
    

    call self % assert(datetime(2014, 1, 2, 4, 4, 5, 6) &
                   >= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime >= datetime (greater)')
    

    call self % assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   >= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime >= datetime (equal)')
    

    call self % assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                    < datetime(2014, 1, 2, 4, 4, 5, 6), &
                    'datetime < datetime')
    

    call self % assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   <= datetime(2014, 1, 2, 4, 4, 5, 6), &
                   'datetime <= datetime (less)')
    

    call self % assert(datetime(2014, 1, 2, 3, 4, 5, 6) &
                   <= datetime(2014, 1, 2, 3, 4, 5, 6), &
                   'datetime <= datetime (equal)')
    

    call self % printBreakLine()

    ! Test datetime % isoformat()

    a = datetime(2014, 1, 1, 0, 0, 0, 0)

    call self % assert(a % isoformat() == '2014-01-01T00:00:00.000', &
                      'datetime % isoformat,  default separator')
    

    call self % assert(a % isoformat('T') == '2014-01-01T00:00:00.000', &
                      'datetime % isoformat,  T separator')
    

    call self % assert(a % isoformat(' ') == '2014-01-01 00:00:00.000', &
                      'datetime % isoformat,  blank separator')
    

    call self % printBreakLine()

    ! Test datetime % strftime() and strptime()

    a = datetime(2014, 1, 2, 3, 4, 5)

    call self % assert(a % strftime('%Y-%m-%d %H:%M:%S')&
                      == '2014-01-02 03:04:05',         &
                      'datetime % strftime')
    

    call self % assert(strptime('2014-01-02 03:04:05', '%Y-%m-%d %H:%M:%S')&
                   == datetime(2014, 1, 2, 3, 4, 5),                           &
                   'datetime % strptime')
    

    call self % assert(strptime(a % strftime('%Y-%m-%d %H:%M:%S'), &
                               '%Y-%m-%d %H:%M:%S') == a,         &
               'strptime(datetime % strftime(fmt), fmt) == datetime')
    

    call self % printBreakLine()

    ! datetime % isocalendar: test all examples from
    ! http://en.wikipedia.org/wiki/ISO_week_date

    a = datetime(2005, 1, 1)
    call self % assert(ALL(a % isocalendar() == [2004, 53, 6]), &
                     'datetime(2005, 1, 1) % isocalendar() == [2004, 53, 6]')
    

    a = datetime(2005, 1, 2)
    call self % assert(ALL(a % isocalendar() == [2004, 53, 7]), &
                     'datetime(2005, 1, 2) % isocalendar() == [2004, 53, 7]')
    

    a = datetime(2005, 12, 31)
    call self % assert(ALL(a % isocalendar() == [2005, 52, 6]), &
                     'datetime(2005, 12, 31) % isocalendar() == [2005, 52, 6]')
    

    a = datetime(2007, 1, 1)
    call self % assert(ALL(a % isocalendar() == [2007, 1, 1]), &
                     'datetime(2007, 1, 1) % isocalendar() == [2007, 1, 1]')
    

    a = datetime(2007, 12, 30)
    call self % assert(ALL(a % isocalendar() == [2007, 52, 7]), &
                     'datetime(2007, 12, 30) % isocalendar() == [2007, 52, 7]')
    

    a = datetime(2007, 12, 31)
    call self % assert(ALL(a % isocalendar() == [2008, 1, 1]), &
                     'datetime(2007, 12, 31) % isocalendar() == [2008, 1, 1]')
    

    a = datetime(2008, 1, 1)
    call self % assert(ALL(a % isocalendar() == [2008, 1, 2]), &
                     'datetime(2008, 1, 1) % isocalendar() == [2008, 1, 2]')
    

    a = datetime(2008, 12, 28)
    call self % assert(ALL(a % isocalendar() == [2008, 52, 7]), &
                     'datetime(2008, 12, 28) % isocalendar() == [2008, 52, 7]')
    

    a = datetime(2008, 12, 29)
    call self % assert(ALL(a % isocalendar() == [2009, 1, 1]), &
                     'datetime(2008, 12, 29) % isocalendar() == [2009, 1, 1]')
    

    a = datetime(2008, 12, 30)
    call self % assert(ALL(a % isocalendar() == [2009, 1, 2]), &
                     'datetime(2008, 12, 30) % isocalendar() == [2009, 1, 2]')
    

    a = datetime(2008, 12, 31)
    call self % assert(ALL(a % isocalendar() == [2009, 1, 3]), &
                     'datetime(2008, 12, 31) % isocalendar() == [2009, 1, 3]')
    

    a = datetime(2009, 1, 1)
    call self % assert(ALL(a % isocalendar() == [2009, 1, 4]), &
                     'datetime(2009, 1, 1) % isocalendar() == [2009, 1, 4]')
    

    a = datetime(2009, 12, 31)
    call self % assert(ALL(a % isocalendar() == [2009, 53, 4]), &
                     'datetime(2009, 12, 31) % isocalendar() == [2009, 53, 4]')
    

    a = datetime(2010, 1, 1)
    call self % assert(ALL(a % isocalendar() == [2009, 53, 5]), &
                     'datetime(2010, 1, 1) % isocalendar() == [2009, 53, 5]')
    

    a = datetime(2010, 1, 2)
    call self % assert(ALL(a % isocalendar() == [2009, 53, 6]), &
                     'datetime(2010, 1, 2) % isocalendar() == [2009, 53, 6]')
    

    a = datetime(2010, 1, 3)
    call self % assert(ALL(a % isocalendar() == [2009, 53, 7]), &
                     'datetime(2010, 1, 3) % isocalendar() == [2009, 53, 7]')
    

    call self % printBreakLine()

    ! datetime % isValid()

    a = datetime(1, 1, 1)
    call self % assert(a % isValid(), 'datetime(1, 1, 1) is valid')
    

    a = datetime(0, 1, 1)
    call self % assert(.not. a % isValid(), 'datetime(0, 1, 1) is not valid')
    

    a = datetime(-1, 1, 1)
    call self % assert(.not. a % isValid(), 'datetime(-1, 1, 1) is not valid')
    

    a = datetime(2014, 1, 1)
    call self % assert(a % isValid(), 'datetime(2014, 1, 1) is valid')
    

    a = datetime(2014, 0, 1)
    call self % assert(.not. a % isValid(), 'datetime(2014, 0, 1) is not valid')
    

    a = datetime(2014, 1, 0)
    call self % assert(.not. a % isValid(), 'datetime(2014, 1, 0) is not valid')
    

    a = datetime(2014, 2, 28)
    call self % assert(a % isValid(), 'datetime(2014, 2, 28) is valid')
    

    a = datetime(2014, 2, 29)
    call self % assert(.not. a % isValid(), 'datetime(2014, 2, 29) is not valid')
    

    a = datetime(2012, 2, 29)
    call self % assert(a % isValid(), 'datetime(2012, 2, 29) is valid')
    

    a = datetime(2012, 3, 31)
    call self % assert(a % isValid(), 'datetime(2012, 3, 31) is valid')
    

    a = datetime(2012, 3, 32)
    call self % assert(.not. a % isValid(), 'datetime(2012, 3, 32) is not valid')
    

    a = datetime(2012, 3, 31, 0, 0, 0)
    call self % assert(a % isValid(), 'datetime(2012, 3, 31, 0, 0, 0) is valid')
    

    a = datetime(2012, 3, 31, 24, 0, 0)
    call self % assert(.not. a % isValid(), 'datetime(2012, 3, 31, 24, 0, 0) is not valid')
    

    a = datetime(2012, 3, 31, 0, 60, 0)
    call self % assert(.not. a % isValid(), 'datetime(2012, 3, 31, 0, 60, 0) is not valid')
    

    a = datetime(2012, 3, 31, 0, 0, 60)
    call self % assert(.not. a % isValid(), 'datetime(2012, 3, 31, 0, 0, 60) is not valid')
    

    a = datetime(2012, 3, 31, 0, 0, 0, 1000)
    call self % assert(.not. a % isValid(), 'datetime(2012, 3, 31, 0, 0, 0, 1000) is not valid')
    

    call self % printBreakLine()

    ! datetime % secondsSinceEpoch
    a = datetime(1970, 1, 1, 0, 0, 0)

    ! First get local machine offset in seconds
    tzOffset = a % secondsSinceEpoch()

    call self % assert(a % secondsSinceEpoch()-tzOffset == 0, &
                      'datetime % secondsSinceEpoch(),  0 seconds')
    

    a = datetime(1970, 1, 1, 1, 0, 0)
    call self % assert(a % secondsSinceEpoch()-tzOffset == 3600, &
                      'datetime % secondsSinceEpoch(),  1 hour')
    

    a = datetime(1969, 12, 31, 23, 0, 0)
    call self % assert(a % secondsSinceEpoch()-tzOffset == -3600, &
                      'datetime % secondsSinceEpoch(),  -1 hour')
    

    call self % printBreakLine()

    ! datetime % tzOffset

    a = datetime(2014, 1, 1, 0, 0, 0, tz=0._real64)
    call self % assert(a % tzOffset() == '+0000', &
                      'datetime % tzOffset(),  +0000')
    

    a = datetime(2014, 1, 1, 0, 0, 0, tz=-3.5_real64)
    call self % assert(a % tzOffset() == '-0330', &
                      'datetime % tzOffset(),  -0330')
    

    a = datetime(2014, 1, 1, 0, 0, 0, tz=5.75_real64)
    call self % assert(a % tzOffset() == '+0545', &
                      'datetime % tzOffset(),  +0545')
    

    call self % printBreakLine()

    ! datetime % utc()

    a = datetime(2014, 1, 1, 0, 0, 0, tz=0._real64)
    call self % assert(a % utc() == a, 'datetime % utc(),  +0000')
    

    !a = datetime(2014, 1, 1, 0, 0, 0, tz=6.)
    !b = a-timedelta(hours=6)
    !b % tz = 0
    !call self % assert(a % utc() == b, 'datetime % utc(),  +0600')
    !

    !a = datetime(2014, 1, 1, 0, 0, 0, tz=-6.)
    !b = a+timedelta(hours=6)
    !b % tz = 0
    !call self % assert(a % utc() == b, 'datetime % utc(),  -0600')
    !!

    call self % printBreakLine()

    ! datetime % weekday()

    a = datetime(2014, 1, 1)
    call self % assert(a % weekday() == 3, 'datetime % weekday(),  Wednesday')
    

    a = datetime(2014, 1, 2)
    call self % assert(a % weekday() == 4, 'datetime % weekday(),  Thursday')
    

    a = datetime(2014, 1, 3)
    call self % assert(a % weekday() == 5, 'datetime % weekday(),  Friday')
    

    a = datetime(2014, 1, 4)
    call self % assert(a % weekday() == 6, 'datetime % weekday(),  Saturday')
    

    a = datetime(2014, 1, 5)
    call self % assert(a % weekday() == 0, 'datetime % weekday(),  Sunday')
    

    a = datetime(2014, 1, 6)
    call self % assert(a % weekday() == 1, 'datetime % weekday(),  Monday')
    

    a = datetime(2014, 1, 7)
    call self % assert(a % weekday() == 2, 'datetime % weekday(),  Tuesday')
    

    call self % printBreakLine()

    ! datetime % isoweekday()

    a = datetime(2014, 1, 1)
    call self % assert(a % isoweekday() == 3, 'datetime % isoweekday(),  Wednesday')
    

    a = datetime(2014, 1, 2)
    call self % assert(a % isoweekday() == 4, 'datetime % isoweekday(),  Thursday')
    

    a = datetime(2014, 1, 3)
    call self % assert(a % isoweekday() == 5, 'datetime % isoweekday(),  Friday')
    

    a = datetime(2014, 1, 4)
    call self % assert(a % isoweekday() == 6, 'datetime % isoweekday(),  Saturday')
    

    a = datetime(2014, 1, 5)
    call self % assert(a % isoweekday() == 7, 'datetime % isoweekday(),  Sunday')
    

    a = datetime(2014, 1, 6)
    call self % assert(a % isoweekday() == 1, 'datetime % isoweekday(),  Monday')
    

    a = datetime(2014, 1, 7)
    call self % assert(a % isoweekday() == 2, 'datetime % isoweekday(),  Tuesday')
    

    call self % printBreakLine()

    ! datetime % weekdayLong()

    a = datetime(2014, 1, 1)
    call self % assert(a % weekdayLong() == 'Wednesday', &
                      'datetime % weekdayLong(),  Wednesday')
    

    a = datetime(2014, 1, 2)
    call self % assert(a % weekdayLong() == 'Thursday', &
                      'datetime % weekdayLong(),  Thursday')
    

    a = datetime(2014, 1, 3)
    call self % assert(a % weekdayLong() == 'Friday', &
                      'datetime % weekdayLong(),  Friday')
    

    a = datetime(2014, 1, 4)
    call self % assert(a % weekdayLong() == 'Saturday', &
                      'datetime % weekdayLong(),  Saturday')
    

    a = datetime(2014, 1, 5)
    call self % assert(a % weekdayLong() == 'Sunday', &
                      'datetime % weekdayLong(),  Sunday')
    

    a = datetime(2014, 1, 6)
    call self % assert(a % weekdayLong() == 'Monday', &
                      'datetime % weekdayLong(),  Monday')
    

    a = datetime(2014, 1, 7)
    call self % assert(a % weekdayLong() == 'Tuesday', &
                      'datetime % weekdayLong(),  Tuesday')
    

    call self % printBreakLine()

    ! datetime % isoweekdayLong()

    a = datetime(2014, 1, 1)
    call self % assert(a % isoweekdayLong() == 'Wednesday', &
                      'datetime % isoweekdayLong(),  Wednesday')
    

    a = datetime(2014, 1, 2)
    call self % assert(a % isoweekdayLong() == 'Thursday', &
                      'datetime % isoweekdayLong(),  Thursday')
    

    a = datetime(2014, 1, 3)
    call self % assert(a % isoweekdayLong() == 'Friday', &
                      'datetime % isoweekdayLong(),  Friday')
    

    a = datetime(2014, 1, 4)
    call self % assert(a % isoweekdayLong() == 'Saturday', &
                      'datetime % isoweekdayLong(),  Saturday')
    

    a = datetime(2014, 1, 5)
    call self % assert(a % isoweekdayLong() == 'Sunday', &
                      'datetime % isoweekdayLong(),  Sunday')
    

    a = datetime(2014, 1, 6)
    call self % assert(a % isoweekdayLong() == 'Monday', &
                      'datetime % isoweekdayLong(),  Monday')
    

    a = datetime(2014, 1, 7)
    call self % assert(a % isoweekdayLong() == 'Tuesday', &
                      'datetime % isoweekdayLong(),  Tuesday')
    

    call self % printBreakLine()

    ! datetime % weekdayShort()

    a = datetime(2014, 1, 1)
    call self % assert(a % weekdayShort() == 'Wed', &
                      'datetime % weekdayShort(),  Wed')
    

    a = datetime(2014, 1, 2)
    call self % assert(a % weekdayShort() == 'Thu', &
                      'datetime % weekdayShort(),  Thu')
    

    a = datetime(2014, 1, 3)
    call self % assert(a % weekdayShort() == 'Fri', &
                      'datetime % weekdayShort(),  Fri')
    

    a = datetime(2014, 1, 4)
    call self % assert(a % weekdayShort() == 'Sat', &
                      'datetime % weekdayShort(),  Sat')
    

    a = datetime(2014, 1, 5)
    call self % assert(a % weekdayShort() == 'Sun', &
                      'datetime % weekdayShort(),  Sun')
    

    a = datetime(2014, 1, 6)
    call self % assert(a % weekdayShort() == 'Mon', &
                      'datetime % weekdayShort(),  Mon')
    

    a = datetime(2014, 1, 7)
    call self % assert(a % weekdayShort() == 'Tue', &
                      'datetime % weekdayShort(),  Tue')
    

    call self % printBreakLine()

    ! datetime % isoweekdayShort()

    a = datetime(2014, 1, 1)
    call self % assert(a % isoweekdayShort() == 'Wed', &
                      'datetime % isoweekdayShort(),  Wed')
    

    a = datetime(2014, 1, 2)
    call self % assert(a % isoweekdayShort() == 'Thu', &
                      'datetime % isoweekdayShort(),  Thu')
    

    a = datetime(2014, 1, 3)
    call self % assert(a % isoweekdayShort() == 'Fri', &
                      'datetime % isoweekdayShort(),  Fri')
    

    a = datetime(2014, 1, 4)
    call self % assert(a % isoweekdayShort() == 'Sat', &
                      'datetime % isoweekdayShort(),  Sat')
    

    a = datetime(2014, 1, 5)
    call self % assert(a % isoweekdayShort() == 'Sun', &
                      'datetime % isoweekdayShort(),  Sun')
    

    a = datetime(2014, 1, 6)
    call self % assert(a % isoweekdayShort() == 'Mon', &
                      'datetime % isoweekdayShort(),  Mon')
    

    a = datetime(2014, 1, 7)
    call self % assert(a % isoweekdayShort() == 'Tue', &
                      'datetime % isoweekdayShort(),  Tue')
    

    call self % printBreakLine()

    ! datetime(gregorian) % yearday()

    a = datetime(2014, 1, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 1, &
                      'datetime(2014, 1, 1, gregorian) % yearday() == 1')
    

    a = datetime(2014, 2, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 32, &
                      'datetime(2014, 2, 1, gregorian) % yearday() == 32')
    

    a = datetime(2014, 3, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 60, &
                      'datetime(2014, 3, 1, gregorian) % yearday() == 60')
    

    a = datetime(2014, 4, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 91, &
                      'datetime(2014, 4, 1, gregorian) % yearday() == 91')
    

    a = datetime(2014, 5, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 121, &
                      'datetime(2014, 5, 1, gregorian) % yearday() == 121')
    

    a = datetime(2014, 6, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 152, &
                      'datetime(2014, 6, 1, gregorian) % yearday() == 152')
    

    a = datetime(2014, 7, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 182, &
                      'datetime(2014, 7, 1, gregorian) % yearday() == 182')
    

    a = datetime(2014, 8, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 213, &
                      'datetime(2014, 8, 1, gregorian) % yearday() == 213')
    

    a = datetime(2014, 9, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 244, &
                      'datetime(2014, 9, 1, gregorian) % yearday() == 244')
    

    a = datetime(2014, 10, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 274, &
                      'datetime(2014, 10, 1, gregorian) % yearday() == 275')
    

    a = datetime(2014, 11, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 305, &
                      'datetime(2014, 11, 1, gregorian) % yearday() == 305')
    

    a = datetime(2014, 12, 1, cal_type=cal_dt_gregorian)
    call self % assert(a % yearday() == 335, &
                      'datetime(2014, 12, 1, gregorian) % yearday() == 335')
    

    call self % printBreakLine()

    ! datetime(noleap) % yearday()

    a = datetime(2014, 1, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 1, &
                      'datetime(2014, 1, 1, noleap) % yearday() == 1')
    

    a = datetime(2012, 2, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 32, &
                      'datetime(2012, 2, 1, noleap) % yearday() == 32')
    

    a = datetime(2014, 3, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 60, &
                      'datetime(2014, 3, 1, noleap) % yearday() == 60')
    

    a = datetime(2014, 4, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 91, &
                      'datetime(2014, 4, 1, noleap) % yearday() == 91')
    

    a = datetime(2014, 5, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 121, &
                      'datetime(2014, 5, 1, noleap) % yearday() == 121')
    

    a = datetime(2014, 6, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 152, &
                      'datetime(2014, 6, 1, noleap) % yearday() == 152')
    

    a = datetime(2014, 7, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 182, &
                      'datetime(2014, 7, 1, noleap) % yearday() == 182')
    

    a = datetime(2014, 8, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 213, &
                      'datetime(2014, 8, 1, noleap) % yearday() == 213')
    

    a = datetime(2014, 9, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 244, &
                      'datetime(2014, 9, 1, noleap) % yearday() == 244')
    

    a = datetime(2014, 10, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 274, &
                      'datetime(2014, 10, 1, noleap) % yearday() == 275')
    

    a = datetime(2014, 11, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 305, &
                      'datetime(2014, 11, 1, noleap) % yearday() == 305')
    

    a = datetime(2014, 12, 1, cal_type=cal_dt_noleap)
    call self % assert(a % yearday() == 335, &
                      'datetime(2014, 12, 1, noleap) % yearday() == 335')
    

    call self % printBreakLine()

    ! datetime(360d) % yearday()

    a = datetime(2014, 1, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 1, &
                      'datetime(2014, 1, 1, 360d) % yearday() == 1')
    

    a = datetime(2012, 2, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 31, &
                      'datetime(2012, 2, 1, 360d) % yearday() == 31')
    

    a = datetime(2014, 3, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 61, &
                      'datetime(2014, 3, 1, 360d) % yearday() == 61')
    

    a = datetime(2014, 4, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 91, &
                      'datetime(2014, 4, 1, 360d) % yearday() == 91')
    

    a = datetime(2014, 5, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 121, &
                      'datetime(2014, 5, 1, 360d) % yearday() == 121')
    

    a = datetime(2014, 6, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 151, &
                      'datetime(2014, 6, 1, 360d) % yearday() == 151')
    

    a = datetime(2014, 7, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 181, &
                      'datetime(2014, 7, 1, 360d) % yearday() == 181')
    

    a = datetime(2014, 8, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 211, &
                      'datetime(2014, 8, 1, 360d) % yearday() == 211')
    

    a = datetime(2014, 9, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 241, &
                      'datetime(2014, 9, 1, 360d) % yearday() == 241')
    

    a = datetime(2014, 10, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 271, &
                      'datetime(2014, 10, 1, 360d) % yearday() == 271')
    

    a = datetime(2014, 11, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 301, &
                      'datetime(2014, 11, 1, 360d) % yearday() == 301')
    

    a = datetime(2014, 12, 1, cal_type=cal_dt_360d)
    call self % assert(a % yearday() == 331, &
                      'datetime(2014, 12, 1, 360d) % yearday() == 331')
    

    ! datetime() % toString()

    a = datetime(2014, 1, 1)
!    write(*,*) "datetime_test:: a= '", a % toString(), "'"
    call self % assert(a % toString() == "2014-01-01 00:00:00 0000 Gregorian", &
          "datetime(2014, 1, 1) % toString() == '2014-01-01 00:00:00 0000 Gregorian'")
    

    a = datetime(2014, 10, 11)
!    write(*,*) "datetime_test:: a='", a % toString(), "'"
    call self % assert(a % toString() == "2014-10-11 00:00:00 0000 Gregorian", &
          'datetime(2014, 10, 11) % toString() == 2014-10-11 00:00:00 0000 Gregorian')

    call self % printBreakLine()
    
    ! datetime copy

    a = datetime(2000,1,1, cal_type=CAL_DT_NOLEAP)
    b = a
    call self % assert(a % getCalendarType() == CAL_DT_NOLEAP, &
          'date(2000,1,1,noleap) % getCalendarType .eq. CAL_DT_NOLEAP == T')
    call self % assert(b % getCalendarType() == CAL_DT_NOLEAP, &
          'b=date(2000,1,1,noleap) % getCalendarType .eq. CAL_DT_NOLEAP == T (after copy)')

    call self % printBreakLine()


    ! Timedelta tests
    call self % assert(timedelta() == timedelta(0, 0, 0, 0, 0), &
                      'timedelta empty constructor')
    

    td = timedelta(milliseconds=1)
    call self % assert(td % total_seconds() >= 1D-3-eps &
                .AND. td % total_seconds() <= 1D-3+eps, &
                'timedelta % total_seconds(),  milliseconds conversion')
    

    td = timedelta(seconds=1)
    call self % assert(td % total_seconds() == 1, &
                     'timedelta % total_seconds(),  seconds conversion')
    

    td = timedelta(minutes=1)
    call self % assert(td % total_seconds() == 60, &
                     'timedelta % total_seconds(),  minutes conversion')
    

    td = timedelta(hours=1)
    call self % assert(td % total_seconds() == 3600, &
                     'timedelta % total_seconds(),  hours conversion')
    

    td = timedelta(days=1)
    call self % assert(td % total_seconds() == 86400, &
                     'timedelta % total_seconds(),  days conversion')
    

    td = timedelta(months=1)
    call self % assert(td % total_seconds() == 86400*30, &
                     'timedelta % total_seconds(),  months conversion')
    

    td = timedelta(years=1)
    call self % assert(td % total_seconds() == 86400*365, &
                     'timedelta % total_seconds(),  years conversion')

    td = timedelta(days=1) * 10
    call self % assert(td == timedelta(days=10), &
                     'timedelta(days=1) * 10 .eq. timedelta(days=10) = T')

    td = timedelta(days=10)
!    write(*,*) "datetime_test:: td='", td % toString(), "'"
    call self % assert(trim(adjustl(td % toString())) == "10d", &
                     'timedelta(days=10) % toString .eq. 10d = T')

    td = timedelta(days=10, minutes=20)
!    write(*,*) "datetime_test:: td='", trim(adjustl(td % toString())), "'"
    call self % assert(trim(adjustl(td % toString())) == "10d 20m", &
                     "timedelta(days=10, minutes=20) % toString .eq. '10d 20m' = T")

    td = timedelta(years=10, minutes=20)
!    write(*,*) "datetime_test:: td='", trim(adjustl(td % toString())), "'"
    call self % assert(trim(adjustl(td % toString())) == "10y 20m", &
                     "timedelta(years=10, minutes=20) % toString .eq. '10y 20m' = T")


    call self % assert(timedelta(days=2) / timedelta(hours=6) == 8., &
                     "timedelta(days=2) % timedelta(hours=6) .eq. 8 = T")

    copytd = td
    call self % assert(copytd == td, &
                     "copytd = timedelta(years=10, minutes=20) = T")


    call self % printBreakLine()

    ! Test date2num and num2date

    a = a % now()
    call self % assert(a % utc() == num2date(date2num(a)), &
                      'datetime % utc() == num2date(date2num(datetime)) (now)')
    

    ! Test for overflowing month
    a = datetime(2014, 11, 30, 1)
    call self % assert(a == num2date(date2num(a)), &
                      'datetime == num2date(date2num(datetime)) (overflowing month)')
    

    ! Test for overflowing year
    a = datetime(2014, 12, 31, 1)
    call self % assert(a == num2date(date2num(a)), &
                      'datetime == num2date(date2num(datetime)) (overflowing year)')
    

    call self % printBreakLine()

    ! datetimeRange tests

    allocate(dtRange(3))

    dtRange = [datetime(2014, 1, 1), &
               datetime(2014, 1, 2), &
               datetime(2014, 1, 3)]
    call self % assert(ALL(datetimeRange(datetime(2014, 1, 1), &
                                        datetime(2014, 1, 3), &
                                        timedelta(days=1)) &
                          == dtRange),                      &
                      'datetimeRange,  day increment')
    

    dtRange = [datetime(2014, 1, 1, 0), &
               datetime(2014, 1, 1, 1), &
               datetime(2014, 1, 1, 2)]
    call self % assert(ALL(datetimeRange(datetime(2014, 1, 1, 0), &
                                        datetime(2014, 1, 1, 2), &
                                        timedelta(hours=1))  &
                          == dtRange),                        &
                      'datetimeRange,  hour increment')
    

    dtRange = [datetime(2014, 1, 1, 0, 0), &
               datetime(2014, 1, 1, 0, 1), &
               datetime(2014, 1, 1, 0, 2)]
    call self % assert(ALL(datetimeRange(datetime(2014, 1, 1, 0, 0), &
                                        datetime(2014, 1, 1, 0, 2), &
                                        timedelta(minutes=1))  &
                          == dtRange),                          &
                      'datetimeRange,  minute increment')
    

    dtRange = [datetime(2014, 1, 1, 0, 0, 0), &
               datetime(2014, 1, 1, 0, 0, 1), &
               datetime(2014, 1, 1, 0, 0, 2)]
    call self % assert(ALL(datetimeRange(datetime(2014, 1, 1, 0, 0, 0), &
                                        datetime(2014, 1, 1, 0, 0, 2), &
                                       timedelta(seconds=1))    &
                          == dtRange),                            &
                      'datetimeRange,  second increment')
    

    deallocate(dtRange)
    allocate(dtRange(7))

    dtRange = [datetime(2012, 1, 1, 0, 0, 0), &
               datetime(2012, 1, 1, 1, 0, 0), &
               datetime(2012, 1, 1, 2, 0, 0), &
               datetime(2012, 1, 1, 3, 0, 0), &
               datetime(2012, 1, 1, 4, 0, 0), &
               datetime(2012, 1, 1, 5, 0, 0), &
               datetime(2012, 1, 1, 6, 0, 0)]
    call self % assert(ALL(datetimeRange(datetime(2012, 1, 1, 0, 0, 0),        &
                                        datetime(2012, 1, 1, 6, 0, 0),        &
                                        timedelta(hours=1)) == dtRange) &
                      .AND. SIZE(datetimeRange(datetime(2012, 1, 1, 0, 0, 0), &
                                               datetime(2012, 1, 1, 6, 0, 0), &
                                               timedelta(hours=1)))     &
                         == SIZE(dtRange), 'datetimeRange,  rounding test')
    

    call self % printBreakLine()

    ! isLeapYear(gregorian) tests
    cal = calendar()
    call self % assert(.not. cal % isLeapYear(1), 'cal(gregorian) isLeapYear(1) == F')
    

    call self % assert(cal % isLeapYear(4), 'cal(gregorian) isLeapYear(4) == T')
    

    call self % assert(.not. cal % isLeapYear(100), 'cal(gregorian) isLeapYear(100) == F')
    

    call self % assert(cal % isLeapYear(400), 'cal(gregorian) isLeapYear(400) == T')
    

    call self % assert(cal % isLeapYear(2000), 'cal(gregorian) isLeapYear(2000) == T')
    

    call self % assert(.not. cal % isLeapYear(2014), 'cal(gregorian) isLeapYear(2014) == F')
    

    call self % printBreakLine()

    ! isLeapYear(no leap) tests
    cal = calendar(cal_dt_noleap)
    call self % assert(.not. cal % isLeapYear(1), 'cal(noleap) % isLeapYear(1) == F')
    

    call self % assert(.not. cal % isLeapYear(4), 'cal(noleap) % isLeapYear(4) == F')
    

    call self % assert(.not. cal % isLeapYear(100), 'cal(noleap) % isLeapYear(100) == F')
    

    call self % assert(.not. cal % isLeapYear(400), 'cal(noleap) % isLeapYear(400) == F')
    

    call self % assert(.not. cal % isLeapYear(2000), 'cal(noleap) % isLeapYear(2000) == F')
    

    call self % assert(.not. cal % isLeapYear(2014), 'cal(noleap) % isLeapYear(2014) == F')
    

    call self % printBreakLine()

    ! isLeapYear(cal_360d) tests
    cal = calendar(CAL_DT_360D)
    call self % assert(.not. cal % isLeapYear(1), 'cal(360d) % isLeapYear(1) == F')
    

    call self % assert(.not. cal % isLeapYear(4), 'cal(360d) % isLeapYear(4) == F')
    

    call self % assert(.not. cal % isLeapYear(100), 'cal(360d) % isLeapYear(100) == F')
    

    call self % assert(.not. cal % isLeapYear(400), 'cal(360d) % isLeapYear(400) == F')
    

    call self % assert(.not. cal % isLeapYear(2000), 'cal(360d) % isLeapYear(2000) == F')
    

    call self % assert(.not. cal % isLeapYear(2014), 'cal(360d) % isLeapYear(2014) == F')
    

    call self % printBreakLine()


    ! daysInYear(gregorian)

    cal = calendar(cal_dt_gregorian)
    call self % assert(cal % getDaysInYear(2014) == 365, 'cal(gregorian) daysInYear(2014) == 365')
    

    call self % assert(cal % getDaysInYear(2012) == 366, 'cal(gregorian) daysInYear(2012) == 366')
    

    call self % assert(cal % getDaysInYear(2000) == 366, 'cal(gregorian) daysInYear(2000) == 366')
    

    call self % assert(cal % getDaysInYear(1900) == 365, 'cal(gregorian) daysInYear(1900) == 365')
    

    call self % printBreakLine()

    ! daysInYear(noleap)

    cal = calendar(cal_dt_noleap)
    call self % assert(cal % getDaysInYear(2014) == 365, 'cal(noleap) daysInYear(2014) == 365')
    

    call self % assert(cal % getDaysInYear(2012) == 365, 'cal(noleap) daysInYear(2012) == 365')
    

    call self % assert(cal % getDaysInYear(2000) == 365, 'cal(noleap) daysInYear(2000) == 365')
    

    call self % assert(cal % getDaysInYear(1900) == 365, 'cal(noleap) daysInYear(1900) == 365')
    

    call self % printBreakLine()

    ! daysInYear(360d)

    cal = calendar(cal_dt_360d)
    call self % assert(cal % getDaysInYear(2014) == 360, 'cal(360d) daysInYear(2014) == 360')
    

    call self % assert(cal % getDaysInYear(2012) == 360, 'cal(360d) daysInYear(2012) == 360')
    

    call self % assert(cal % getDaysInYear(2000) == 360, 'cal(360d) daysInYear(2000) == 360')
    

    call self % assert(cal % getDaysInYear(1900) == 360, 'cal(360d) daysInYear(1900) == 360')
    

    call self % printBreakLine()

    ! daysInMonth(gregorian)

    cal = calendar(cal_dt_gregorian)
    call self % assert(cal % getDaysInMonth(1, 2014) == 31, 'cal(gregorian) daysInMonth(1, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(2, 2014) == 28, 'cal(gregorian) daysInMonth(2, 2014) == 28')
    

    call self % assert(cal % getDaysInMonth(2, 2012) == 29, 'cal(gregorian) daysInMonth(2, 2012) == 29')
    

    call self % assert(cal % getDaysInMonth(3, 2014) == 31, 'cal(gregorian) daysInMonth(3, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(4, 2014) == 30, 'cal(gregorian) daysInMonth(4, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(5, 2014) == 31, 'cal(gregorian) daysInMonth(5, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(6, 2014) == 30, 'cal(gregorian) daysInMonth(6, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(7, 2014) == 31, 'cal(gregorian) daysInMonth(7, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(8, 2014) == 31, 'cal(gregorian) daysInMonth(8, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(9, 2014) == 30, 'cal(gregorian) daysInMonth(9, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(10, 2014) == 31, 'cal(gregorian) daysInMonth(10, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(11, 2014) == 30, 'cal(gregorian) daysInMonth(11, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(12, 2014) == 31, 'cal(gregorian) daysInMonth(12, 2014) == 31')
    

    call self % printBreakLine()

    ! daysInMonth(noleap)

    cal = calendar(cal_dt_noleap)
    call self % assert(cal % getDaysInMonth(1, 2014) == 31, 'cal(noleap) daysInMonth(1, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(2, 2014) == 28, 'cal(noleap) daysInMonth(2, 2014) == 28')
    

    call self % assert(cal % getDaysInMonth(2, 2012) == 28, 'cal(noleap) daysInMonth(2, 2012) == 28')
    

    call self % assert(cal % getDaysInMonth(3, 2014) == 31, 'cal(noleap) daysInMonth(3, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(4, 2014) == 30, 'cal(noleap) daysInMonth(4, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(5, 2014) == 31, 'cal(noleap) daysInMonth(5, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(6, 2014) == 30, 'cal(noleap) daysInMonth(6, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(7, 2014) == 31, 'cal(noleap) daysInMonth(7, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(8, 2014) == 31, 'cal(noleap) daysInMonth(8, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(9, 2014) == 30, 'cal(noleap) daysInMonth(9, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(10, 2014) == 31, 'cal(noleap) daysInMonth(10, 2014) == 31')
    

    call self % assert(cal % getDaysInMonth(11, 2014) == 30, 'cal(noleap) daysInMonth(11, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(12, 2014) == 31, 'cal(noleap) daysInMonth(12, 2014) == 31')
    

    call self % printBreakLine()

    ! daysInMonth(360d)

    cal = calendar(cal_dt_360d)
    call self % assert(cal % getDaysInMonth(1, 2014) == 30, 'cal(360d) daysInMonth(1, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(2, 2014) == 30, 'cal(360d) daysInMonth(2, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(2, 2012) == 30, 'cal(360d) daysInMonth(2, 2012) == 30')
    

    call self % assert(cal % getDaysInMonth(3, 2014) == 30, 'cal(360d) daysInMonth(3, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(4, 2014) == 30, 'cal(360d) daysInMonth(4, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(5, 2014) == 30, 'cal(360d) daysInMonth(5, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(6, 2014) == 30, 'cal(360d) daysInMonth(6, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(7, 2014) == 30, 'cal(360d) daysInMonth(7, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(8, 2014) == 30, 'cal(360d) daysInMonth(8, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(9, 2014) == 30, 'cal(360d) daysInMonth(9, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(10, 2014) == 30, 'cal(360d) daysInMonth(10, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(11, 2014) == 30, 'cal(360d) daysInMonth(11, 2014) == 30')
    

    call self % assert(cal % getDaysInMonth(12, 2014) == 30, 'cal(360d) daysInMonth(12, 2014) == 30')

    call self % printBreakLine()

    ! copy(360d)

    cal = calendar(cal_dt_360d)
    cpcal = cal
    call self % assert(cal % getType() == CAL_DT_360D, 'cal(360d) % getCalendarType() == CAL_DT_360D')
    call self % assert(cpcal % getType() == CAL_DT_360D, 'cal(360d) % getCalendarType() == CAL_DT_360D (after copy)')


    call self % printBreakLine()

    ! clock tests
    ! Restart clock with a daily tick interval 
    c = clock(datetime(2014, 1, 1, 0, 0, 0), &
              datetime(2015, 1, 2, 0, 0, 0), &
              timedelta(days=1), &
              startFromTime  = datetime(2014, 10, 1, 0, 0, 0))
    call self % assert(c % currentTime == datetime(2014, 10, 1, 0, 0, 0), &
                'clock % currentTime  == datetime(2014, 10, 1)')
    
    call self % assert(c % prevTime == datetime(2014, 9, 30, 0, 0, 0), & 
                'clock % prevTime  == datetime(2014, 9, 30)')
    
    call self % assert(c % nextTime == datetime(2014, 10, 2, 0, 0, 0), &
                'clock % nextTime  == datetime(2014, 10, 2)')
    

    call c % tick()
    call c % tick()

    call self % assert(c % currentTime == datetime(2014, 10, 3, 0, 0, 0), &
                'clock % currentTime  == datetime(2014, 10, 3)')
    
    call self % assert(c % prevTime == datetime(2014, 10, 2, 0, 0, 0), &
                'clock % prevTime  == datetime(2014, 10, 2)')
    
    call self % assert(c % nextTime == datetime(2014, 10, 4, 0, 0, 0), &
                'clock % nextTime  == datetime(2014, 10, 4)')
    
    
    !

    ! Initialize a clock with an hourly tick interval
    c = clock(startTime    = datetime(2014, 1, 1, 0, 0, 0), &
              stopTime     = datetime(2014, 1, 2, 0, 0, 0), &
              currentTime  = datetime(2014, 1, 1, 0, 0, 0), &
              tickInterval = timedelta(hours=1))

    call self % assert(.not. c % started, 'clock % started == F (before tick)')
    

    call c % tick()

    call self % assert(c % started, 'clock % started == T (after 1 tick)')
    

    call self % assert(.not. c % stopped, 'clock % stopped == F (after 1 tick)')
    

    ! Tick 23 times
    do i = 1, 23
      call c % tick()
    end do

    call self % assert(c % currentTime == c % stopTime, &
                      'clock % currentTime == clock % stopTime (after 24 ticks)')
    

    call self % assert(c % isStopped(), 'clock % isStopped == T (after 24 ticks)')
    

    ! Reset clock
    call c % reset()

    call self % assert(.not. c % started, 'clock % started == F (after reset)')
    

    call self % assert(.not. c % started, 'clock % stopped == F (after reset)')
    

    call self % assert(c % currentTime == c % startTime, &
                      'clock % currentTime == clock % startTime (after reset)')
    

    call self % printBreakLine()

    ! String representation
    call self % assert(c % toString() == &
            "2014-01-01 00:00:00 0000 Gregorian <- 2014-01-01 00:00:00 0000 Gregorian -> 2014-01-02 00:00:00 0000 Gregorian", & 
            "clock % toString == 2014-01-01 00:00:00 0000 Gregorian <- "// &
            "2014-01-01 00:00:00 0000 Gregorian -> 2014-01-02 00:00:00 0000 Gregorian")

    call self % printBreakLine()

    ! copy clock (use cal_type argument )

    c = clock(datetime(2014, 1, 1, cal_type=CAL_DT_NOLEAP), &
              datetime(2015, 1, 2, cal_type=CAL_DT_NOLEAP), &
              timedelta(days=1), &
              startFromTime  = datetime(2014, 10, 1, 0, 0, 0, cal_type=CAL_DT_NOLEAP))
    cp = c
    call self % assert(c % startTime % getCalendarType() == CAL_DT_NOLEAP, &
                      'c % starTime % getCalendarType() .eq. CAL_DT_NOLEAP == T')
    call self % assert(cp % startTime % getCalendarType() == CAL_DT_NOLEAP, &
                      'cp % startTime % getCalendarType() .eq. CAL_DT_NOLEAP == T')

    call self % printBreakLine()

    ! copy clock (clock caltype argument has priority)

    c = clock(datetime(2014, 1, 1, cal_type=CAL_DT_NOLEAP), &
              datetime(2015, 1, 2, cal_type=CAL_DT_NOLEAP), &
              timedelta(days=1), &
              caltype=CAL_DT_360D, &
              startFromTime  = datetime(2014, 10, 1, 0, 0, 0, cal_type=CAL_DT_360D))
    cp = c
!    write(*,*) "datetime_test:: c % startTime % getCalendarType() =", c % startTime % getCalendarType()
    call self % assert(c % startTime % getCalendarType() == CAL_DT_360D, &
                      'c % starTime % getCalendarType() .eq. CAL_DT_360D == T')
    call self % assert(cp % startTime % getCalendarType() == CAL_DT_360D, &
                      'cp % startTime % getCalendarType() .eq. CAL_DT_360D == T')
    

    call self % printBreakLine()

    ! isBeginYear, isEndYear
    c = clock(datetime(2013, 12, 31, 0, 0, 0), &
              datetime(2015, 1, 2, 0, 0, 0), &
              timedelta(days=1))
    call self % assert(.not. c % isBeginYear(), "clock(2013,12,31) % isBeginYear() == F" )
    
    call self % assert(c % isEndYear(), "clock(2013,12,31) % isBeginYear() == T" )
    

    call c % tick()

    call self % assert(c % isBeginYear(), "clock(2014,1,1) % isBeginYear() == T (after 1 ticks)" )
    
    call self % assert(.not. c % isEndYear(), "clock(2014,1,1) % isBeginYear() == F (after 1 ticks)" )
    

    call c % tick()

    call self % assert(.not. c % isBeginYear(), "clock(2014,1,2) % isBeginYear() == F (after 2 ticks)" )
    
    call self % assert(.not. c % isEndYear(), "clock(2014,1,2) % isBeginYear() == F (after 2 ticks)" )
    

    call self % printBreakLine()

    ! isBeginMonth, isEndMonth 
    c = clock(datetime(2014, 1, 31, 0, 0, 0), &
              datetime(2015, 1, 2, 0, 0, 0), &
              timedelta(days=1))

    call self % assert(.not. c % isBeginMonth(), "clock(2014,1,31) % isBeginMonth() == F " )
    
    call self % assert(c % isEndMonth(), "clock(2014,1,31) % isBeginMonth() ==  T" )
    

    call c % tick()

    call self % assert( c % isBeginMonth(), "clock(2014,2,1) % isBeginMonth() == T (after 1 tick)" )
    
    call self % assert(.not. c % isEndMonth(), "clock(2014,2,1) % isBeginMonth() ==  F (after 1 tick)" )
    

    call c % tick()

    call self % assert(.not. c % isBeginMonth(), "clock(2014,2,1) % isBeginMonth() == F (after 2 ticks)" )
    
    call self % assert(.not. c % isEndMonth(), "clock(2014,2,1) % isBeginMonth() ==  F (after 2 ticks)" )
    

    call self % printBreakLine()

    ! isBeginDay, isEndDay 
    c = clock(datetime(2014, 1, 1, 23, 30, 0), &
              datetime(2015, 2, 1, 0, 0, 0), &
              timedelta(minutes=30))

    call self % assert(.not. c % isBeginDay(), "clock(2014,1,31, 23:30) % isBeginDay() == T " )
    
    call self % assert(c % isEndDay(), "clock(2014,1,31, 23:30) % isBeginDay() ==  T" )
    

    call c % tick()

    call self % assert( c % isBeginDay(), "clock(2014,1,1, 00:00) % isBeginDay() == T (after 1 tick)" )
    
    call self % assert(.not. c % isEndDay(), "clock(2014,0,1, 00:00) % isBeginDay() ==  F (after 1 tick)" )
    

    call c % tick()

    call self % assert(.not. c % isBeginDay(), "clock(2014,1,1, 00:30) % isBeginDay() == F (after 2 ticks)" )
    
    call self % assert(.not. c % isEndDay(), "clock(2014,1,1, 00:30) % isBeginDay() ==  F (after 2 ticks)" )
    

    call self % printBreakLine()

    ! isBeginClock, isEndClock
    c = clock(datetime(2014, 1, 1, 0, 0, 0), &
              datetime(2014, 1, 3, 0, 0, 0), &
              timedelta(days=1))

    call self % assert( c % isBeginClock(), "clock(2014,1,1) % isBeginClock() == T" )
    
    call self % assert(.not. c % isEndClock(), "clock(2014,1,1) % isBeginClock() ==  F" )
    

    call c % tick()

    call self % assert(.not. c % isBeginClock(), "clock(2014,1,2) % isBeginClock() == F" )
    
    call self % assert(.not. c % isEndClock(), "clock(2014,1,2) % isBeginClock() ==  F" )
    

    call c % tick()

    call self % assert(.not. c % isBeginClock(), "clock(2014,1,3) % isBeginClock() == F" )
    
    call self % assert( c % isEndClock(), "clock(2014,1,3) % isBeginClock() ==  T" )

    ! totalTickIntervals
    call self % assert( c % totalTickIntervals() == 2, &
            "clock(2014,1,1 to 2014,1,2) % totalTickIntervals() .eq. 2 ==  T" )


    ! findEquivalentDatetime
    clock1day = clock(startTime=datetime(1900, 1, 1), &
              stopTime=datetime(1901, 1, 1), &
              currentTime=datetime(1900, 1, 6), & ! current time
              tickInterval=timedelta(days=1))


    Clock2days = clock(starttime=datetime(1900, 1, 1), &
              stopTime=datetime(1901, 1, 1), &
              currentTime=datetime(1900, 1, 11), & ! current time
              tickInterval=timedelta(days=2))


    ! test to itself (same ts)
    call self % assert( clock1day % findEquivalentCurrentTime(clock1day) == datetime(1900, 1, 6), &
            "c % findEquivalentCurrentTime(c) .eq. dt(1900, 1, 6) == T" )


    ! model has longuer ts than forcing
    tmpdt = clock1day % findEquivalentCurrentTime(Clock2days)
    !write (*,*) "datetime_test:: ", tmpdt % toString()
    call self % assert( clock1day % findEquivalentCurrentTime(Clock2days) == datetime(1900, 1, 11), &
            "c % findEquivalentCurrentTime(clock2days) .eq. dt(1900, 1, 11) == T" )


    ! model has shorter ts than forcing
    tmpdt = clock2days % findEquivalentCurrentTime(Clock1day)
    !write (*,*) "datetime_test:: ", tmpdt % toString()
    call self % assert( Clock2days % findEquivalentCurrentTime(clock1day) == datetime(1900, 1, 5), &
            "clock2days % findEquivalentCurrentTime(clock1day) .eq. dt(1900, 1, 5) == T" )


    ! ==
    clock1day = clock(startTime=datetime(1900, 1, 1), &
              stopTime=datetime(1901, 1, 1), &
              currentTime=datetime(1900, 1, 6), & ! current time
              tickInterval=timedelta(days=1))
    call self % assert( clock1day == clock1day, &
            "clock1day .eq. clock1day == T" )

    call self % assert( .not. (clock1day == clock2days), &
            "clock1day .eq. clock2days == F" )


  end subroutine test_datetime

end module datetime_test

