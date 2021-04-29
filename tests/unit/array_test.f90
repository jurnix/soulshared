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
  use testSuite_mod, only: testSuite
  use precision_mod, only: sp

  use array_mod, only: initArrayRange

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


  end subroutine defineTestCases

end module array_test

