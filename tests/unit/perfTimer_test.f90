!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : perfTimer_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> perfTimer unit tests
!------------------------------------------------------------------------------
module perfTimer_test
  use system_mod, only: FortSleep
  use precision_mod, only: dp
  use perfTimer_mod, only: perfTimer
  use testSuite_mod, only: testSuite

  use, intrinsic :: iso_c_binding, only: c_int

  implicit none

  private
  public :: testSuitePerfTimer

  type, extends(testSuite) :: testSuitePerfTimer

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine run_nseconds(pTimer, tid, secs)
    !< simulation pTimer captures times for 'secs' seconds
    type(perfTimer), intent(inout) :: pTimer
    integer, intent(in) :: tid
    real(kind=dp), intent(in) :: secs

    pTimer % allTimers(tid) % startWall =  1.0
    pTimer % allTimers(tid) % endWall = secs + pTimer % allTimers(tid) % startWall
  end subroutine run_nseconds


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuitePerfTimer), intent(inout) :: self

    type(perfTimer) :: pTimer
    integer :: sec_timer, twosec_timer
    real(kind=dp) :: wallSec, wallTwosec
    integer (c_int) :: wait_secs, how_long
    character(len=:), allocatable :: tmp

    ! init
    pTimer = perfTimer()
    sec_timer = pTimer % add("sec", "1 second timer")
    twosec_timer = pTimer % add("two", "2 second timer")

    tmp = pTimer % toString(sec_timer)
!    write(*,*) "perfTimer_test:: define:: sec_timer id,  init, =", sec_timer, tmp 
!    write(*,*) "perfTimer_test:: define:: twosec_timer id =", twosec_timer
    call self % assert(pTimer % toString(sec_timer) == &
                        "Performance timer: sec (1 second timer) = - (init)", &
                        "pTimer % toString() .eq. '... - init'  = T")

    ! run simulation
    call pTimer % setStart(sec_timer)
    call pTimer % setStart(twosec_timer)

    wait_secs = 1
    how_long = FortSleep(wait_secs)

    tmp = pTimer % toString(sec_timer)
!    write(*,*) "perfTimer_test:: define:: sec_timer running =", sec_timer, tmp 
    call self % assert(pTimer % toString(sec_timer) == &
                        "Performance timer: sec (1 second timer) = - (running)", &
                        "pTimer % toString() .eq. '... - init'  = T")

    ! counter done
    call pTimer % setEnd(sec_timer)
    how_long = FortSleep(wait_secs)

    ! counter done
    call pTimer % setEnd(twosec_timer)

    ! done
    wallSec = pTimer % getWallTime(sec_timer)
    wallTwosec = pTimer % getWallTime(twosec_timer)
    call self % assert(wallSec >= 1.0 , "1 second timer = T")
    call self % assert(wallTwoSec >= 2.0 , "2 second timer = T")

    call run_nseconds(pTimer, sec_timer, 1.0_dp) ! exact time
    call self % assert(pTimer % toString(sec_timer) == &
                        "Performance timer: sec (1 second timer) = 1.0000000000000000    secs", &
                        "pTimer % toString() .eq. '... 1 secs'  = T")

    tmp = pTimer % toString(sec_timer)
!    write(*,*) "perfTimer_test:: define:: wallSec secs, done =", wallSec, tmp
!    write(*,*) "perfTimer_test:: define:: wallTwosec secs =", wallTwosec 

  end subroutine defineTestCases

end module perfTimer_test

