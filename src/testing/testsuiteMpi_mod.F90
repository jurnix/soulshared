!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : SHR_testSuiteMpi_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> a testSuiteMpi contains multiple unit tests:
!> Parallel (mpi) tests
!> Sequential tests are only support for processor MPI_ROOT_PROC
!------------------------------------------------------------------------------
module SHR_testSuiteMpi_mod 
  use SHR_strings_mod, only: int2string, string, stringCollection
  use SHR_testSuiteMpiContext_mod, only: testSuiteMpiContext, MPI_ROOT_PROC
  use SHR_testSuite_mod, only: testSuite
  use SHR_error_mod, only: raiseError

  implicit none

  private
  public :: testSuiteMpi

  logical, parameter :: ISDEBUG = .false.


  type, abstract, extends(testSuite) :: testSuiteMpi
    logical, allocatable :: mpiResults(:) !< discretize result for each proc
    integer :: currMpiTests !< total number of parallel tests

    type(testSuiteMpiContext) :: mpiCtxt
    integer :: myid                           !< current processor id
    integer :: npes                           !< total number of procs
  contains
    procedure :: initMpi => initTestSuiteMpi 
    procedure :: report                       !< print an overview of the resultsç
    procedure :: passert                      !< mpi parallel testCase
    procedure :: run                          !< run all test cases
    procedure :: isSuccessful                 !< true whether all tests are successful
  end type testSuiteMpi

contains


  subroutine passert(self, condition, test_name)
    !< parallel assert (called by all mpi procs)
    class(testSuiteMpi), intent(in out) :: self
    logical, intent(in) :: condition
    character(*), intent(in) :: test_name
    character(60) :: output_test_name

    integer :: i
    logical, allocatable :: allConditions(:)
    integer, allocatable :: failedPes(:)
    type(string), allocatable :: failedPesStr(:)
    type(stringCollection) :: str
    integer, allocatable :: procsId(:)

    if (.not. allocated (self % mpiResults) ) then
      write(*,*) "=========== Error found =========== "
      write(*,*) "Filename: "//__FILE__
      write(*,*) "Subroutine: assert"
      write(*,*) "Message: testSuiteMpi not initialized"
      stop
    endif

    output_test_name = test_name

    call self % mpiCtxt % gather(condition, allConditions)

    if (self % mpiCtxt % isRootProc() ) then !self % myid == MPI_ROOT_PROC) then
      if (ISDEBUG) write (*,*) "testSuiteMpi_mod:: assert:: allconditions =", allConditions

      if (all(allConditions)) then
        print *, 'test ' // output_test_name // ': ' // &
          char(27) // '[32mPASS' // char(27) // '[0m'
      else
        ! discover which procs failed
        allocate(procsId(self % npes)) ! todo - initArrayRange
        procsId = [ (i+1, i = 0, self % npes-1) ]
        failedPes = pack( procsId, .not. allConditions)
        if (ISDEBUG) write (*,*) "testSuiteMpi_mod:: assert:: failedPes =", failedPes

        ! ints to string
        failedPesStr = int2string(failedPes)
        str = stringCollection(failedPesStr)

        print '(a)', 'test ' // output_test_name // ': ' // &
          char(27) // '[31mFAIL (' // str % toString() // ')' // char(27) // '[0m'
      end if
    endif

    ! add new test result
    self % currMpiTests = self % currMpiTests + 1
    if (self  % currTests .gt. size(self % mpiResults)) then
      call raiseError(__FILE__, "assert", &
          "Reached the number of tests allowed in testCase "// self % name, & 
           "Increase them to avoid this error") 
    end if
    self % mpiResults(self % currMpiTests) = condition
    if (ISDEBUG) write (*,*) self % myid, ":: testSuiteMpi_mod:: current mpi tests done = ", self % currMpiTests
    if (ISDEBUG) write (*,*) self % myid, ":: testSuiteMpi_mod:: mpiResults = ", self % mpiResults(self % currMpiTests)
    if (ISDEBUG) write (*,*) self % myid, ":: testSuiteMpi_mod:: assert:: done"

  end subroutine passert


  subroutine initTestSuiteMpi(self, name, ntestsToDo, mpiComm, mpiInfo)
    class(testSuiteMpi), intent(in out) :: self
    character(len=*), intent(in) :: name
    integer, intent(in) :: ntestsToDo
    integer, intent(in) :: mpiComm
    integer, intent(in) :: mpiInfo

    ! todo (proper override) - copy from testSuite % initTestSuite
    if (allocated(self % results)) deallocate(self % results)
    allocate(self % results(ntestsToDo))
    self % results(:) = .false.
    self % ntests = ntestsToDo
    self % currTests = 0
    self % currMpiTests = 0
    self % name = name
    self % nsuccess = 0
    self % nfailure = 0

    allocate(self % mpiResults(ntestsToDo))

    self % mpiResults = .false.
    self % mpiCtxt = testSuiteMpiContext(mpiComm, mpiInfo)
    self % npes = self % mpiCtxt % getSize()
    self % myId = self % mpiCtxt % getRank()

    if (ISDEBUG) write(*,*) "testSuiteMpi_test:: initTestSuiteMpi:: myid =", self % myid
    if (ISDEBUG) write(*,*) "testSuiteMpi_test:: initTestSuiteMpi:: npes =", self % npes
  end subroutine initTestSuiteMpi


  subroutine report(self)
    class(testSuiteMpi), intent(in out) :: self
    ! Takes the test counter as input and reports 
    ! the total number of test passes and failures.
    !ilogical, optional, intent(out) :: test_failed

    character(len=60) :: str, str1

    logical, allocatable :: allMpiResults(:)

    integer :: rootTestsSize
    logical, allocatable :: tmp(:)
    integer :: itest
    integer :: seqFailure, seqSuccess
    integer :: mpiFailure, mpiSuccess
    integer :: allCurrentTests

    rootTestsSize = self % npes * self % currMpiTests

    allocate(allMpiResults(self % currMpiTests))

    if (ISDEBUG) write(*,*) self % myid, ":: unittestMpi_mod:: report:: mpiResults=", self % mpiResults(1:self % currMpiTests)
!    if (ISDEBUG) write(*,*) self % myid, ":: unittestMpi_mod:: report:: rootTestsSize=", rootTestsSize

    ! collect results from other procs
    do itest = 1 , self % currMpiTests 
      if (ISDEBUG) write(*,*) self % myid, ":: unittestMpi_mod:: report:: itest, mpi result=", &
                                        itest, self % mpiResults(itest)
      call self % mpiCtxt % gather(self % mpiResults(itest), tmp)
      if (ISDEBUG) write(*,*) self % myid, ":: unittestMpi_mod:: report:: itest, mpi result gathered=", &
                                        tmp

      ! results from each proc are Ok?
      ! if yes, successful unit test
      ! else no, failed unit test
      allMpiResults(itest) = all(tmp .eqv. .true.)
    enddo
    if (ISDEBUG) write(*,*) self % myid, ":: unittestMpi_mod:: do done"

    if (ISDEBUG) then
      if (self % mpiCtxt % isRootProc() ) &
              write(*,*) self % myid, ":: unittestMpi_mod:: report:: allMpiResults=", allMpiResults(1:self % currMpiTests)
    endif

    if (self % myid == MPI_ROOT_PROC) then
      allCurrentTests = self % currMpiTests + self % currTests
      if (ISDEBUG) write(*,*) "testSuiteMpi_mod:: report:: total tests done (seq + par)=", allCurrentTests

      ! sequential tests
      seqSuccess = count(self % results(1:self % currTests))
      seqFailure = count(self % results(1:self % currTests) .eqv. .false.)
      ! parallel tests
      mpiSuccess = count(allMpiResults)
      mpiFailure = count(allMpiResults .eqv. .false.)
      ! all tests
      self % nsuccess = mpiSuccess + seqSuccess 
      self % nfailure = mpiFailure + seqFailure

      write (str, '(a, a, i3, a)') self % name, ' ran a total of ', allCurrentTests, ' tests :'
      write (str1, '(i3, a, i3, a)') self % nsuccess, ' tests PASSED,  ', &
                            self % nfailure, ' tests FAILED.'

      print '(a, a)', str, trim(str1)
    endif

  end subroutine report


  logical function isSuccessful (self)
    class(testSuiteMpi), intent(in out) :: self
    ! All testcases are successful

    if ( self % mpiCtxt % isRootProc()) isSuccessful = (self % nfailure == 0)
    call self % mpiCtxt % bcast(isSuccessful)

  end function isSuccessful


  subroutine run(self) 
    ! Run all defined tests
    class(testSuiteMpi), intent(in out) :: self

    if (self % mpiCtxt % isRootProc() ) then
      print *
      print '(20("-"), a, 20("-"))', " "//self % name//" "
    endif

    ! run defined tests 
    call self % define()

    if (self % mpiCtxt % isRootProc()) print '(71("-"))'

  end subroutine run

end module SHR_testSuiteMpi_mod
