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
module testSuite_mod 
  use iso_fortran_env, only: real64
  use error_mod, only: raiseError

  implicit none

  private
  public :: testSuite 


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
    character(60) :: output_test_name

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
!    write(*,*) "assert:: currTests=", self % currTests
!    write(*,*) "assert:: size ( results )", size(self % results)
    self % results(self % currTests) = condition

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
    character(len=60) :: str, str1

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

    print '(71("-"))'

  end subroutine run


  subroutine printBreakLine(self)
    !< print a new break line into the screen
    class(testSuite), intent(in out) :: self

    print '(71("-"))'
  end subroutine printBreakLine

end module testSuite_mod
