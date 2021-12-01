!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : testSuite_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> a testSuite contains multiple unit tests. 
!------------------------------------------------------------------------------
module SHR_testSuite_mod 
  use iso_fortran_env, only: real64
  use SHR_error_mod, only: raiseError

  use shr_objects_mod, only: shr_wrapObject

  implicit none

  private
  public :: testSuite 

  integer, parameter :: MAX_TEST_NAME = 60


  type, abstract :: testSuite
    character(len=:), allocatable :: name
    integer :: ntests                         !< max number of tests
    integer :: nsuccess, nfailure             !< total number of successfull/failed tests
    integer :: currTests                      !< current number of tests
    logical, allocatable :: results(:)        !< tests results (success or failure)
  contains
    procedure :: init => initTestSuite 
    procedure :: report                       !< print an overview of the results
    procedure :: assert                       !< testCase
    procedure :: printBreakLine               !< print a new breakline
    procedure :: run                          !< run all test cases
    procedure(define_iface), deferred :: define !< define tests cases
    procedure :: isSuccessful                 !< true whether all tests are successful

    !< assertions
    procedure :: assertTrueAlloc_r1
    generic :: assertTrueAlloc => assertTrueAlloc_r1
    procedure :: assertTrue_r1
    generic :: assertTrue => assertTrue_r1
  end type testSuite

  interface 
    subroutine define_iface( self )
       import testSuite
       class(testSuite), intent(inout) :: self
    end subroutine define_iface 
  end interface 

contains


  subroutine assert(self, condition, test_name)
    class(testSuite), intent(in out) :: self
    logical, intent(in) :: condition
    character(*), intent(in) :: test_name
    character(MAX_TEST_NAME) :: output_test_name

    if (.not. allocated (self % results) ) then
      write(*,*) "=========== Error found =========== "
      write(*,*) "Filename: "//__FILE__
      write(*,*) "Subroutine: assert"
      write(*,*) "Message: testSuite not initialized"
      stop
    endif

    output_test_name = test_name

    if (condition) then
      print *, 'test ' // output_test_name // ': ' // &
        char(27) // '[32mPASS' // char(27) // '[0m'
    else
      print '(a)', 'test ' // output_test_name // ': ' // &
        char(27) // '[31mFAIL' // char(27) // '[0m'
    end if

    ! add new test result
    self % currTests = self % currTests + 1
    if (self  % currTests .gt. size(self % results)) then
      call raiseError(__FILE__, "assert", &
          "Reached the number of tests allowed in testCase "// self % name, & 
           "Increase them to avoid this error") 
    end if
    self % results(self % currTests) = condition
!    write(*,*) "assert:: current sequential tests done =", self % currTests
!    write(*,*) "assert:: sequential results =", self % results(1:self % currTests)
!    write(*,*) "assert:: done"

  end subroutine assert


  subroutine initTestSuite(self, name, ntestsToDo)
    class(testSuite), intent(in out) :: self
    character(len=*), intent(in) :: name
    integer, intent(in) :: ntestsToDo

    if (allocated(self % results)) deallocate(self % results)
    allocate(self % results(ntestsToDo))
    self % results(:) = .false.
    self % ntests = ntestsToDo
    self % currTests = 0
    self % name = name
    self % nsuccess = 0
    self % nfailure = 0
  end subroutine initTestSuite


  subroutine report(self)
    class(testSuite), intent(in out) :: self
    ! Takes the test counter as input and reports 
    ! the total number of test passes and failures.
    !ilogical, optional, intent(out) :: test_failed

    integer :: n, ntests
    character(len=80) :: str, str1

    ntests = self % currTests !size(slef % results)

    self % nsuccess = 0
    self % nfailure = 0
    do n = 1, ntests
      if (self % results(n)) then
        self % nsuccess = self % nsuccess + 1
      else
        self % nfailure = self % nfailure + 1
      end if
    end do

    write (str, '(a, a, i3, a)') self % name, ' ran a total of ', ntests, ' tests :'
    write (str1, '(i3, a, i3, a)') self % nsuccess, ' tests PASSED,  ', &
                            self % nfailure, ' tests FAILED.'

    print '(a, a)', str, trim(str1)

  end subroutine report


  logical function isSuccessful (self)
    class(testSuite), intent(in out) :: self
    ! All testcases are successful

    isSuccessful = (self % nfailure == 0)

  end function isSuccessful


  subroutine run(self) 
    ! Run all defined tests
    class(testSuite), intent(in out) :: self

    print *
    print '(20("-"), a, 20("-"))', " "//self % name//" "

    ! run defined tests 
    call self % define()

    call self % printBreakLine()

  end subroutine run


  subroutine printBreakLine(self)
    !< print a new break line into the screen
    class(testSuite), intent(in out) :: self

    print '(71("-"))'
  end subroutine printBreakLine


  subroutine assertTrueAlloc_r1(self, expected, found, test_name) 
    !< assert true if expected and found have the same size,
    !< same values and are both allocated
    !< otherwise false
    class(testSuite), intent(in out) :: self
    type(shr_wrapObject), allocatable, intent(in) :: expected(:)
    type(shr_wrapObject), allocatable, intent(in) :: found(:)
    character(*), intent(in) :: test_name

    if (.not. allocated(expected)) then
      call self % assert(.false., test_name)
      return
    endif

    if (.not. allocated(found)) then
      call self % assert(.false., test_name)
      return
    endif

    call self % assertTrue(expected, found, test_name)
  end subroutine assertTrueAlloc_r1


  subroutine assertTrue_r1(self, expected, found, test_name) 
    !< assert true if expected and found have the same size,
    !< and same values. Otherwise false.
    class(testSuite), intent(in out) :: self
    type(shr_wrapObject), intent(in) :: expected(:)
    type(shr_wrapObject), intent(in) :: found(:)
    character(*), intent(in) :: test_name

    ! same size?
    if (size(expected) /= size(found)) then ! nope 
      call self % assert(.false., test_name)
    else ! yes
      ! same elements?
      call self % assert(all(expected == found), test_name)
    endif 
  end subroutine assertTrue_r1

end module SHR_testSuite_mod
