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
module array_test
  use SHR_testSuite_mod, only: testSuite
  use SHR_precision_mod, only: sp

  use SHR_array_mod, only: initArrayRange, trimArrayIndex

  implicit none

  private
  public :: testSuiteArray


  type, extends(testSuite) :: testSuiteArray
    contains
      procedure :: define => defineTestCases
  end type 

contains


  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteArray), intent(inout) :: self

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

  end subroutine defineTestCases

end module array_test

