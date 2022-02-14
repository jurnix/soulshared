!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :   shr_gridDomainPartitioningMethodBySameNumberOfGridcells_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> grid domain partitioning method by same number of gridcells unit test
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethodBySquares_test

  use shr_testSuite_mod, only: testSuite
  use shr_gridDomainPartitioningMethodBySquares_mod, only: &
          shr_gridDomainPartitioningMethodBySquares

  !< factory methods


  implicit none

  private
  public :: testSuitegridDomainPartitioningMethodBySquares

  type, extends(testSuite) :: testSuitegridDomainPartitioningMethodBySquares

  contains
    procedure :: define => defineTestCases
    procedure, private :: testCaseWithThree
  end type
contains



  subroutine defineTestCases(self)
    !< test
    use iso_c_binding
    class(testSuitegridDomainPartitioningMethodBySquares), intent(inout) :: self

    call self % testCaseWithThree()
  end subroutine defineTestCases


  subroutine testCaseWithThree(self)
    !<
    !> 8 enabled gridcells into 3 parts:
    !>               (domain 1)  (domain 2)    (domain 3)  (domain 4)
    !>    (5)   (1)
    !> (4) x x - x     x x - -     - - - x
    !>     x x x x  ->   ...    -> - - - -  ->  x x x x ->
    !>     x x x x                              x x x x
    !> (0) - - x -                                         - - x -
    class(testSuitegridDomainPartitioningMethodBySquares), intent(inout) :: self

  end subroutine testCaseWithThree


end module shr_gridDomainPartitioningMethodBySquares_test