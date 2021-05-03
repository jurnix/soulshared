!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_perfTimer_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> Performance timer class
!> 
!> It allows to capture wall time or elapsed time
!>
!> Wall time: 4 procs, 1 to 10 seconds -> 10 seconds wall time
!> Elapsed time: 4 procs, 1 to 10 seconds -> 40 seconds elapsed time
!>
!> If MPI available, it uses mpi_wtime (more precise)
!>
!> otherwise it uses Fortran intrinsics
!------------------------------------------------------------------------------
module SHR_perfTimer_mod
  use SHR_precision_mod, only: dp
  use SHR_error_mod, only: raiseError
#ifdef ENABLE_MPI
  use mpi
#endif

  implicit none

  private

  public :: perfTimer

  integer, parameter :: MAX_PERFTIMERS = 20

  integer, parameter :: PERFTIMER_STATUS_INIT = 1
  integer, parameter :: PERFTIMER_STATUS_RUNNING = 2
  integer, parameter :: PERFTIMER_STATUS_DONE = 3

  type uniquePerfTimer
    integer :: id
    character(len=:), allocatable :: name
    character(len=:), allocatable :: description
    real(kind=dp) :: startWall, endWall
    logical :: isCounting !< true if startWall is defined
    logical :: isEmpty !< true if has no name nor description
  contains
    procedure, public :: getStatus
  end type uniquePerfTimer

  interface uniquePerfTimer
    module procedure :: uniquePerfTimer_constructor
  end interface uniquePerfTimer


  type perfTimer
    type(uniquePerfTimer), allocatable :: allTimers(:)
    integer :: idCounter

  contains 
    procedure, public :: add 
    procedure, public :: setStart
    procedure, public :: setEnd
    procedure, public :: getWallTime !< no matter how many cpu's running
    procedure, private :: getNewId 
    procedure, private :: hasId !< true is given id exists
    procedure :: toString => toString_perfTimer

  end type perfTimer

  interface perfTimer
    module procedure :: perfTimer_constructor
  end interface perfTimer

contains

  type(uniquePerfTimer) function uniquePerfTimer_constructor(id, name, description)
    !< uniquePerfTimer constructor
    integer, intent(in) :: id 
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: description
    integer, parameter :: UNDEF_TIMER = -1
    uniquePerfTimer_constructor % isCounting = .false.
    uniquePerfTimer_constructor % isEmpty = .true.
    uniquePerfTimer_constructor % startWall = UNDEF_TIMER
    uniquePerfTimer_constructor % endWall = UNDEF_TIMER
    uniquePerfTimer_constructor % name = name
    uniquePerfTimer_constructor % description = description
    uniquePerfTimer_constructor % id = id
#ifndef ENABLE_DEF
    call raiseError(__FILE__, "uniquePerfTimer_constructor", &
            "MPI not enabled")
#endif
  end function uniquePerfTimer_constructor


  integer function getStatus(self)
    !< returns uniquePerfTimer status (init, running, done)
    class(uniquePerfTimer), intent(inout) :: self

    logical :: isInit, isRunning, isDone
    isInit =    .not. self % isCounting .and.       self % isEmpty
    isRunning =       self % isCounting .and. .not. self % isEmpty
    isDone =    .not. self % isCounting .and. .not. self % isEmpty

!    write(*,*) "perfTimer_mod:: getStatus:: isCounting, isEmpty =", self % isCounting, self % isEmpty

    if (isInit) then
      getStatus = PERFTIMER_STATUS_INIT
    else if (isRunning) then
      getStatus = PERFTIMER_STATUS_RUNNING 
    else if (isDone) then
      getStatus = PERFTIMER_STATUS_DONE
    else
      ! unexpected result
      ! assert(unexpected outcome)
    endif
  end function getStatus


  type(perfTimer) function perfTimer_constructor()
    !< perfTimer constructor
    perfTimer_constructor % idCounter = 0
    allocate(perfTimer_constructor % allTimers(MAX_PERFTIMERS))
  end function perfTimer_constructor


  function add(self, name, description) result (rid)
    !< Add a new timer with 'name' and 'description'.
    !< It returns a unique id for the new perfTimer
    class(perfTimer), intent(inout) :: self
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: description
    integer :: rid !< id given to new perfCounter

    integer :: newId

    newId = self % getNewId()
    self % allTimers(newId) = uniquePerfTimer(newId, name, description)
    rid = newId
  end function add


  logical function hasId(self, id)
    !< true if given 'id' exists in perfTimer
    !< false if outside bounds or not yet assigned (vs idCounter)
    class(perfTimer), intent(in) :: self
    integer, intent(in) :: id

    logical :: isOutsideBounds, isAssigned

    isOutsideBounds = (id < 1 .or. id > MAX_PERFTIMERS)
    isAssigned = (id <= self % idCounter)

    if (isOutsideBounds) then
      hasId = .false.

    else if (.not. isAssigned) then
      hasId = .false.

    else 
      hasId = .true.

    endif
  end function hasId


  integer function getNewId(self)
    !< it returns an id not yet used by any counter
    class(perfTimer), intent(inout) :: self

    if (self % idCounter > MAX_PERFTIMERS) then
      call raiseError(__FILE__, "getNewId", &
              "Maximum number of perfTimers reached")
    endif

    self % idCounter = self % idCounter + 1 
    getNewId = self % idCounter
  end function getNewId


  subroutine setStart(self, id)
    !< it marks when to start counting time for a given timer 'id'
    class(perfTimer), intent(inout) :: self
    integer, intent(in) :: id !< timerId

    if (id < 1 .or. id > MAX_PERFTIMERS)  then
      call raiseError(__FILE__, "setStart", &
              "Given perfTimer id outside bounds")
    endif

    associate ( utimer => self % allTimers(id) )
      utimer % isCounting = .true.
      utimer % isEmpty = .false.
#ifdef ENABLE_MPI
      utimer % startWall = MPI_Wtime() ! in seconds
#endif
    end associate 
  end subroutine setStart


  subroutine setEnd(self, id)
    !< it marks when to end counting time
    class(perfTimer), intent(inout) :: self
    integer, intent(in) :: id !< timerId

    integer :: perfStatus

    if (id < 1 .or. id > MAX_PERFTIMERS)  then
      call raiseError(__FILE__, "setEnd", &
              "Given perfTimer id outside bounds")
    endif

    associate ( utimer => self % allTimers(id) )
      perfStatus = utimer % getStatus()
      if (perfStatus == PERFTIMER_STATUS_INIT) then 
        call raiseError(__FILE__, "setStart", &
             "Given perfTimer must be running") 
      endif

      if (perfStatus == PERFTIMER_STATUS_DONE) then
        call raiseError(__FILE__, "setEnd", &
          "Given perfTimer has already done its job") 
      endif

#ifdef ENABLE_MPI
      utimer % endWall = MPI_Wtime() ! in seconds
#endif
      utimer % isCounting = .false.
    end associate
  end subroutine setEnd


  real(kind=dp) function getWallTime(self, id)
    !< 
    class(perfTimer), intent(inout) :: self
    integer, intent(in) :: id !< timerId

    associate ( utimer => self % allTimers(id) )
      getWallTime = utimer % endWall - utimer % startWall  ! in seconds
    end associate
  end function getWallTime


  function toString_perfTimer(self, id) result (r)
    !< string representation
    !< 
    !< (status) 
    !< 
    !< (init) Performance timer: name ( description ) = - ( init )
    !< (running) Performance timer: name ( description ) = - ( running )
    !< (done) Performance timer: name ( description ) = 10.12 sec
    class(perfTimer), intent(inout) :: self
    integer, intent(in) :: id !< perfTimer id
    character(len=:), allocatable :: r
    logical :: isInit, isRunning, isDone

    real(kind=dp) :: diffTime
    character(len=40) :: tmp
    character(len=:), allocatable :: name, descrp
    integer :: perfStatus

    associate ( utimer => self % allTimers(id) )
      ! get status
      perfStatus = utimer % getStatus()
      isInit =    ( perfStatus == PERFTIMER_STATUS_INIT )
      isRunning = ( perfStatus == PERFTIMER_STATUS_RUNNING )
      isDone =    ( perfStatus == PERFTIMER_STATUS_DONE )

!      write(*,*) "perfTimer:: toString_perfTimer:: status =", perfStatus, isInit, isRunning, isDone

      name = utimer % name
      descrp = utimer % description
      if (isInit) then 
        r = "Performance timer: "// name //" ("//descrp//") = - (init)"
      else if (isRunning) then 
        r = "Performance timer: "// name //" ("//descrp//") = - (running)"
      else if (isDone) then 
        diffTime = self % getWallTime(id) 
        write(tmp, *) diffTime
        r = "Performance timer: "// name //" ("//descrp//") = " // adjustl(trim(tmp)) // " secs"
      else
        call raiseError(__FILE__, "toString_perfTimer", "Unexpected outcome")
      endif
    end associate

  end function toString_perfTimer

end module SHR_perfTimer_mod
