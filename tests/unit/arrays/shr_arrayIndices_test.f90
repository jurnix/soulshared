!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : shr_arrayIndices_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> 
!------------------------------------------------------------------------------
module shr_arrayIndices_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp

  use shr_arrayIndices_mod, only: shr_arrayGridcellIndex

  implicit none

  private
  public :: testSuiteArrayIndices


  type, extends(testSuite) :: testSuiteArrayIndices

    contains
      procedure :: define => defineTestCases
      procedure, private :: testCaseArrayGridcellIndex
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArrayIndices), intent(inout) :: self

    call self % testCaseArrayGridcellIndex()

  end subroutine defineTestCases


  subroutine testCaseArrayGridcellIndex(self)
    !< shr_arrayGridcellIndex unit test
    class(testSuiteArrayIndices), intent(inout) :: self
    type(shr_arrayGridcellIndex) :: agci, agciBig

    ! [1,2]
    agci % row = 1
    agci % col = 2
    call self % assert ( agci == [1,2], &
        "shr_arrayGridcellIndex(1,2) .eq. [1,2] = T"  )
    ! ==
    agciBig % row = 5
    agciBig % col = 3
    call self % assert ( .not. (agci == agciBig), &
        "shr_arrayGridcellIndex(1,2) .eq. shr_arrayGridcellIndex(5,3) = F")
    call self % assert (agci == agci, &
        "shr_arrayGridcellIndex(1,2) .eq. shr_arrayGridcellIndex(1,2) = T")
  end subroutine testCaseArrayGridcellIndex

end module shr_arrayIndices_test

