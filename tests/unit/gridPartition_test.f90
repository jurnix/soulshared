!------------------------------------------------------------------------------
!    Pekin University - Land Surface Model 
!------------------------------------------------------------------------------
! PROGRAM        : gridPartition_test 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> grid partitoin unit tests
!------------------------------------------------------------------------------
module gridPartition_test
  use shr_gridPartition_mod, only: grid_partition_type
  use SHR_testSuite_mod, only: testSuite

  implicit none

  private
  public :: testSuiteGridPartition

  type, extends(testSuite) :: testSuiteGridPartition

    contains
      procedure :: define => defineTestCases
  end type 

contains

  subroutine defineTestCases(self)
    use iso_c_binding
    class(testSuiteGridPartition), intent(inout) :: self

    type(grid_partition_type) :: partition
    type(grid_partition_type) :: landPartition
    integer :: partSize
    integer, parameter :: indices(10) = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    integer, parameter :: landIndices(5) = [2, 4, 5, 6, 8]
    integer, allocatable :: locIndices(:)
    integer :: bounds(2)

    partition = grid_partition_type(1, 3, indices)
    call self % assert(.true., "partition(current=2, total=3, elements=[1,2,3,4,5,6,7,8,9,10])")

    ! getPartitionSize
    partSize = partition % getPartitionSize()
    call self % assert(partSize == 3 , "partition % getPartitionSize() .eq 3 == T")

    partSize = partition % getPartitionSize(0)
    call self % assert(partSize == 4, "partition % getPartitionSize(0) .eq 4 == T")

    call self % assert(partition % getPartitionId() == 1, "partition % getCurrentPartition() .eq 2 == T")
    call self % assert(partition % getTotalPartitions() == 3, "partition % getTotalPartition() .eq 3 == T")
    call self % assert(partition % getTotalElements() == 10, "partition % getTotalElements() .eq 10 == T")

    ! getPartitionIndices
    locindices = partition % getPartitionIndices()
    call self % assert(all(locIndices == [5, 6, 7]), "partition % getPartitionIndices() .eq 5,6,7 == T")

    locindices = partition % getPartitionIndices(1)
    call self % assert(all(locIndices == [1,2,3,4]), "partition % getPartitionIndices() .eq 1,2,3,4 == T")

    ! getPartitionBounds
    bounds = partition % getPartitionBounds()
    call self % assert(all(bounds == [5, 7]), "partition % getPartitionBounds() .eq 5,7 == T")

    bounds = partition % getPartitionBounds(1)
    call self % assert(all(bounds == [1, 4]), "partition % getPartitionBounds() .eq 1,4 == T")


    ! grid partition with enabled gridcells
    landPartition = grid_partition_type(1, 3, landIndices) ! partitioning = 2, 2, 1
    partSize = landPartition % getPartitionSize(0)
    call self % assert(partSize == 2 , "partition % getPartitionSize(0) .eq 2 == T")
    partSize = landPartition % getPartitionSize(1)
    call self % assert(partSize == 2, "partition % getPartitionSize(1) .eq 2 == T")
    partSize = landPartition % getPartitionSize(2)
    call self % assert(partSize == 1, "partition % getPartitionSize(2) .eq 1 == T")
     

  end subroutine defineTestCases

end module gridPartition_test

