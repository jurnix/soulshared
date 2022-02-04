!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : array_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> common array subroutines 
!------------------------------------------------------------------------------
module arrayUtils_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp

  use SHR_arrayUtils_mod, only: initArrayRange, trimArrayIndex, shr_arrayCalculatorIndices, shr_arrayGridcellIndex

  implicit none

  private
  public :: testSuiteArrayUtils


  type, extends(testSuite) :: testSuiteArrayUtils
    contains
      procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArrayUtils), intent(inout) :: self
    type(shr_arrayGridcellIndex) :: agci, agciBig

    ! initRealArrayRange
    call self % assert ( all(initArrayRange(1.,2.,0.25) == [1.,1.25,1.5,1.75,2.0]), &
                        "initArrayRange(1.,2.,0.25) .eq. [1.,1.25,1.5,1.75,2.0] == T" )
    call self % assert ( all(initArrayRange(2.,1.,-0.25) == [2.,1.75,1.5,1.25,1.]), &
                        "initArrayRange(2.,1.,-0.25) .eq. [2,1.75,1.5,1.25,1.] == T" )

    ! trimArrayIndex
    call self % assert ( trimArrayIndex( [.false., .false., .true., .true.] ) == 3, &
            "trimArrayIndex( [F F T T] ) == 3"  )
    call self % assert ( trimArrayIndex( [.true., .true., .true., .true.], direction="start" ) == 1, &
            "trimArrayIndex( [T T T T], direction=start ) == 1"  )
    call self % assert ( trimArrayIndex( [.false., .true., .true., .true.], direction="start" ) == 2, &
            "trimArrayIndex( [F T T T], direction=start ) == 2"  )
    call self % assert ( trimArrayIndex( [.false., .true., .false., .true.], direction="end" ) == 4, &
            "trimArrayIndex( [F T F T], direction=end ) == 4"  )
    call self % assert ( trimArrayIndex( [.true., .false., .false., .false.], direction="end" ) == 1, &
            "trimArrayIndex( [T F F F], direction=end ) == 1"  )

    ! shr_arrayCalculatorIndices
    call self % assert ( shr_arrayCalculatorIndices(3,4,7) == [2,3], &
        "shr_arrayCalculatorIndices(3,4,7) .eq. (2,3) = T"  )
    call self % assert ( shr_arrayCalculatorIndices(3,4,11) == [3,3], &
        "shr_arrayCalculatorIndices(3,4,11) .eq. (3,3) = T"  )
    call self % assert ( shr_arrayCalculatorIndices(3,4,1) == [1,1], &
        "shr_arrayCalculatorIndices(3,4,1) .eq. (1,1) = T"  )
    call self % assert ( shr_arrayCalculatorIndices(3,4,12) == [3,4], &
        "shr_arrayCalculatorIndices(3,4,12) .eq. (3,4) = T"  )

    ! shr_arrayGridcellIndex

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

  end subroutine defineTestCases

end module arrayUtils_test

