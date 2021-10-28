!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : gridPartition_mod 
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> It splits a 1d grid domain into N parts
!> 
!> 10 elements into 3 partitions ---> output: 4, 3, 3 partitions
!>
!------------------------------------------------------------------------------
module shr_gridPartition_mod 
  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  implicit none

  public :: shr_gridPartition

  logical, parameter :: ISDEBUG = .false.


  type shr_gridPartition
    integer, allocatable :: total_elements !< 1 to N
    integer, allocatable :: partitions_size(:) !< number of indices in partition 

    integer, allocatable :: gloIdcs(:) !< global grid indices
    integer, allocatable :: startIdxs(:) !< start index for each partition
    integer, allocatable :: endIdxs(:) !< end index for each partition

    integer :: type !< how to partition
    integer :: current_partition !< hold data from this partition (0 to total_partitions-1)
    integer :: total_partitions !< total number of partitions
  contains

    procedure :: getPartitionSize ! (partition number) -> calculate the total number of gridcells for the requested current partition
    procedure :: getPartitionId
    procedure :: getPartitionIndices ! (partition number) -> 
    procedure :: getPartitionBounds ! (partition number) -> 
    procedure :: getTotalPartitions
    procedure :: getTotalElements
  end type shr_gridPartition


  interface shr_gridPartition
    procedure :: shr_gridPartition_initialize
  end interface shr_gridPartition

contains


  type(shr_gridPartition) function shr_gridPartition_initialize(current, total, gridIndices)
    !< grid partition type constructor

    integer, intent(in) :: current !< range  from 0 to n-1
    integer, intent(in) :: total !< range from 1 to N
    integer, intent(in) :: gridIndices(:) !< global indices

    !< local
    integer, allocatable :: grids_per_partition(:)
    integer :: ipart, remaining_grids

    integer :: startidx, endidx
    integer :: totalPartitions

    character(len=20) :: tmp, tmp1

    shr_gridPartition_initialize % current_partition = current
    shr_gridPartition_initialize % total_partitions = total
    shr_gridPartition_initialize % total_elements = size(gridIndices)
    shr_gridPartition_initialize % gloIdcs = gridIndices

!    if (ISDEBUG) write(*,*) "gridPartition_mod:: shr_gridPartition_initialize:: gridIndices =", gridIndices
    if (ISDEBUG) write(*,*) "gridPartition_mod:: shr_gridPartition_initialize:: total partitions =", total
    if (ISDEBUG) write(*,*) "gridPartition_mod:: shr_gridPartition_initialize:: current partition =", current

    if (current >= total) then
      write(tmp,*) current
      write(tmp1,*) total
      call raiseError(__FILE__, "shr_gridPartition_initialize", &
                                "Current partition id must smaller than total partitions", &
                                "Current:"//tmp, &
                                "Total:"//tmp1 )
    endif

    ! only postive number
    if (current < 0) then
      write(tmp,*) current
      call raiseError(__FILE__, "shr_gridPartition_initialize", &
                                "Current partition id must be equal or bigger than 0", &
                                "Current:"//tmp)
    endif

    ! calculate how many grids for each partition
    allocate(grids_per_partition(total))

    ! calculate partitions
    grids_per_partition(:) = size(gridIndices) / total
    remaining_grids = mod(size(gridIndices), total)
    do ipart = 1, remaining_grids
      grids_per_partition(ipart) = grids_per_partition(ipart) + 1
    end do

    if (ISDEBUG) write(*,*) "gridPartition_mod:: shr_gridPartition_initialize:: remaining_grids =", remaining_grids
    if (ISDEBUG) write(*,*) "gridPartition_mod:: shr_gridPartition_initialize:: grids_per_partition=", grids_per_partition
    shr_gridPartition_initialize % partitions_size = grids_per_partition
    if (ISDEBUG) write(*,*) "gridPartition_mod:: shr_gridPartition_initialize:: partition_size=", &
            shr_gridPartition_initialize % partitions_size

    totalPartitions = total

    ! calculate start and end for each partition
    allocate(shr_gridPartition_initialize % startIdxs(totalPartitions) )
    allocate(shr_gridPartition_initialize % endIdxs(totalPartitions) )

    startidx = 1
    endidx = shr_gridPartition_initialize % getPartitionSize(0)
    if (ISDEBUG) write(*,*) "grid_mod:: shr_gridPartition_initialize:: first, start, end =", startIdx, endIdx
    do ipart = 1, totalPartitions
      shr_gridPartition_initialize % startIdxs(ipart) = startidx
      shr_gridPartition_initialize % endIdxs(ipart) = endidx
      startidx = shr_gridPartition_initialize % endIdxs(ipart) + 1

      if (ipart + 1 <= totalPartitions) then
        endidx = startidx + shr_gridPartition_initialize % getPartitionSize(ipart) - 1
      endif
      if (ISDEBUG)  write(*,*) "grid_mod:: grid_domain_type_initialize:: start, end, ipart =", startIdx, endIdx, ipart
    enddo

  end function shr_gridPartition_initialize


  integer function getPartitionSize(self, partition)
    !< it returns the number of elements for the 'current' partition 
    class(shr_gridPartition), intent(in) :: self
    integer, intent(in), optional :: partition !< 0 to N - 1
    character(*), parameter :: SNAME = "getPartitionSize"
    integer :: ipart
    character(len=20) :: tmp, tmp1

    if (present(partition)) then

      if (partition >= self % total_partitions) then
        write(tmp,*) partition
        write(tmp1,*) self % total_partitions
        call raiseError(__FILE__, SNAME, &
                  "Requested partition id must smaller than total partitions", &
                  "Requested: "//tmp, &
                  "Total: "//tmp1)
      endif

      ! only positive number
      if (partition < 0) then
        write(tmp,*) partition
        call raiseError(__FILE__, SNAME, &
                  "Current partition id must be equal or bigger than 0", &
                  "Current:"//tmp)
      endif
      
    endif

    ipart = self % current_partition + 1
    if (present(partition)) ipart = partition + 1

    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionSize:: partitions_size, ipart = ", self % partitions_size, ipart
    getPartitionSize = self % partitions_size(ipart)
  end function getPartitionSize


  integer function getPartitionId(self)
    !< it returns the id from the current partition
    class(shr_gridPartition), intent(in) :: self

    getPartitionId = self % current_partition
  end function getPartitionId


  integer function getTotalPartitions(self)
    !< it returns the number of partitions 
    class(shr_gridPartition), intent(in) :: self
    getTotalPartitions = self % total_partitions
  end function getTotalPartitions


  function getPartitionIndices(self, partition) result (locIndices)
    !< it returns the local indices from 'partition'
    class(shr_gridPartition), intent(in) :: self
    integer, intent(in), optional :: partition
    integer, allocatable :: locIndices(:)

    integer :: idxBounds(2)
    integer :: cpart
    integer :: newSize
    integer :: startIdx, endIdx

    cpart = self % getPartitionId() + 1
    if (present(partition)) cpart = partition

    if (allocated(locIndices)) deallocate(locIndices)
    idxBounds = self % getPartitionBounds(cpart)

    startIdx = idxBounds(1)
    endIdx = idxBounds(2)

    newSize = endIdx - startIdx + 1
    allocate(locIndices(newSize))

    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionIndices:: partition =", cpart
    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionIndices:: newSize =", newSize
    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionIndices:: startidx, endidx =", startIdx, endIdx
    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionIndices:: gloIdcs =", self % gloIdcs
    locIndices(1:newSize) = self % gloIdcs(startIdx:endIdx)
  end function getPartitionIndices


  function getPartitionBounds(self, partition) result (bounds)
    !< it returns the partition bounds (start and end) from global indices
    class(shr_gridPartition), intent(in) :: self
    integer, intent(in), optional :: partition !< between 1 to P
    integer :: bounds(2)

    integer :: cpart

    cpart = self % getPartitionId() + 1
    if (present(partition) ) cpart = partition
    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionBounds:: cpart =", cpart

    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionBounds:: start id =", self % startIdxs ! (cpart)
    if (ISDEBUG) write(*,*) "gridPartition_mod:: getPartitionBbounds:: end id =", self % endIdxs ! (cpart)

    bounds(1) = self % startIdxs(cpart)
    bounds(2) = self % endIdxs(cpart)
  end function getPartitionBounds


  integer function getTotalElements(self)
    !< it returns the total number of elements to distribute
    class(shr_gridPartition), intent(in) :: self
    getTotalElements = self % total_elements
  end function getTotalElements

end module shr_gridPartition_mod 

