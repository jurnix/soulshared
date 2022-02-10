!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> shr_gridDomain is partitioned into the same number of
!> enabledhorizontal consecutive gridcells.
!>
!> 8 enabled gridcells into 3 parts:
!>            (domain 1)  (domain 2)    (domain 3)
!> x x - x     x x - x
!> x - - x  ->          -> x - - x  ->
!> x x - -                 x b b b      b x - -
!> - - x -                              - - x -
!>
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod

  use SHR_error_mod, only: raiseError

  use shr_gridDomainPartitioningMethod_abs, only: shr_gridDomainPartitioningMethod

  use shr_gGrid_mod, only: shr_igGrid
  use shr_gridMask_mod, only: shr_gridMask_cast, shr_IgridMask, shr_gridMask
  use shr_gridDomain_mod, only: shr_gridDomain, shr_iGridDomain

  use shr_gridMaskFindEnabledEqualSplitIterator_mod, only: shr_gridMaskFindEnabledEqualSplitIterator
  use shr_gridMaskFindEnabledEqualSplitMethod_mod, only: shr_gridMaskFindEnabledEqualSplitMethod


  implicit none

  public :: shr_gridDomainPartitioningMethodBySameNumberOfGridcells, &
              GRID_DOMAIN_PARTITION_EQUAL

  logical, parameter :: ISDEBUG = .false.

  character(*), parameter :: GRID_DOMAIN_PARTITION_EQUAL = "equal"


  type, extends(shr_gridDomainPartitioningMethod) :: shr_gridDomainPartitioningMethodBySameNumberOfGridcells
    class(shr_igridDomain), allocatable :: gdomain !< input
    class(shr_igridDomain), allocatable :: newDomains(:) !< output

    type(shr_gridMaskFindEnabledEqualSplitMethod), allocatable :: findEnabledEqualSplitMethod
    integer, allocatable :: total
  contains
    procedure :: init
    procedure :: calculate
    procedure :: get
  end type shr_gridDomainPartitioningMethodBySameNumberOfGridcells

contains

  subroutine init(self, total)
    !< initialize with custom arguments
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self
    integer, intent(in) :: total

    allocate( self % total)
    self % total = total
  end subroutine init


  subroutine calculate(self, domain)
    !< compute partitions
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self
    class(shr_iGridDomain), intent(in) :: domain
    class(shr_IgridMask), allocatable :: enabled
    class(shr_iGridDomain), allocatable :: filteredDomain
    class(shr_iggrid), allocatable :: filteredGrid
    integer :: isplit
    type(shr_gridMask) :: tmpGMask

    allocate(self % gdomain, source = domain)
    allocate(self % newDomains(self % total), mold = domain)
    allocate(self % findEnabledEqualSplitMethod)

    !< calculate earch part
    enabled = self % gdomain % getEnabledGridMask()
    call self % findEnabledEqualSplitMethod % init(self % total, enabled)

    !< over each part
    do isplit = 1, self % findEnabledEqualSplitMethod % getSize()
      !> convert each gridMask into new domain
      tmpGMask = self % findEnabledEqualSplitMethod % get(isplit)
      filteredDomain = self % gdomain % filter(tmpGMask)
      filteredGrid = filteredDomain % getGrid()
      self % newDomains(isplit) = self % gdomain % select(filteredGrid)
    end do
  end subroutine calculate


  function get(self) result (newDomains)
    !< it returns partitioned domains
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(in) :: self
    class(shr_igridDomain), allocatable :: newDomains(:) !< output
    allocate(newDomains, source = self % newDomains)
  end function get

end module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod

