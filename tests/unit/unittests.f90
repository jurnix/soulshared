!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : unittest
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> It runs all tests 
!------------------------------------------------------------------------------

program unittest

  use datetime_test, only: testSuiteDatetime
  use tst_test, only: testSuiteTst
  use strings_test, only: testSuiteStrings
  use dict_test, only: testSuiteDict
  use array_test, only: testSuiteArray
  use perfTimer_test, only: testSuitePerfTimer
  use file_test, only: testSuiteFile

  implicit none

  ! declare each test
  type(testSuiteDatetime) :: tsDatetime
  type(testSuiteTst) :: tsTst
  type(testSuiteStrings) :: tsStrings
  type(testSuiteDict) :: tsDict
  type(testSuiteArray) :: tsArray
  type(testSuitePerfTimer) :: tsPerfTimer
  type(testSuiteFile) :: tsFile

  ! declare tests
  call tsDatetime % init("Datetime test", 320)
  call tsTst % init("Ternary search tree test", 30)
  call tsStrings % init("Strings test", 70)
  call tsDict % init("Dict test", 10)
  call tsArray % init("Array test", 20)
  call tsPerfTimer % init("Performance timer test", 20)
  call tsFile % init("File test", 10)

  ! run all tests 
  call tsDatetime % run()
  call tsTst % run() 
  call tsStrings % run() 
  call tsDict % run() 
  call tsArray % run() 
  call tsPerfTimer % run() 
  call tsFile % run() 


  ! show results
  call tsDatetime % report()
  call tsTst % report() 
  call tsStrings % report() 
  call tsDict % report() 
  call tsArray % report() 
  call tsPerfTimer % report() 
  call tsFile % report()

  ! crash if any of the tests fail
  if (.not. tsDatetime % isSuccessful() .or. &
      .not. tsTst % isSuccessful() .or. &
      .not. tsStrings % isSuccessful() .or. & 
      .not. tsDict % isSuccessful() .or. &
      .not. tsArray % isSuccessful() .or. &
      .not. tsPerfTimer % isSuccessful() .or. &
      .not. tsFile % isSuccessful() &
      ) stop 1

end program unittest
