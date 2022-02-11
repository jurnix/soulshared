!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model
!------------------------------------------------------------------------------
! MODULE        :   shr_gridDomainPartitioningMethodBySameNumberOfGridcells_test
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_test

  use shr_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuitegridDomainPartitioningMethodBySameNumberOfGridcells

  type, extends(testSuite) :: testSuitegridDomainPartitioningMethodBySameNumberOfGridcells

  contains
    procedure :: define => defineTestCases

    procedure, private :: testCaseSimpleConversion
  end type
contains


  subroutine defineTestCases(self)
    !< test
    use iso_c_binding
    class(testSuitegridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self
    call self % testCaseSimpleConversion()
  end subroutine defineTestCases


  subroutine testCaseSimpleConversion(self)
    !< how to partition(algorithm):

    class(testSuitegridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self

      call self % assert(.false., "TODO... = T ")
  end subroutine testCaseSimpleConversion


end module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_test