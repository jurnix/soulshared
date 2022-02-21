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
  use shr_strings_mod, only: string

  use shr_gridDomainPartitioningMethod_abs, only: shr_gridDomainPartitioningMethod

  use shr_gGrid_mod, only: shr_igGrid
  use shr_gridMask_mod, only: shr_gridMask_cast, shr_IgridMask, shr_gridMask
  use shr_gridDomain_mod, only: shr_gridDomain, shr_iGridDomain, shr_gridDomainWrap

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
    class(shr_IgridMask), allocatable :: enabled, enabledGMask
    class(shr_iGridDomain), allocatable :: filteredDomain, selectedGridDomain
    class(shr_iggrid), allocatable :: filteredGrid, shrinkedGrid
    integer :: isplit
    class(shr_igridMask), allocatable :: tmpGMask, tmpGMaskShrinked, tmpBorderMaskedShrinked
!    type(string) :: tmp

    allocate(filteredDomain, mold = domain) !< allocate and get its type
    allocate(self % gdomain, source = domain)
    allocate(self % newDomains(self % total), mold = domain)
    allocate(self % findEnabledEqualSplitMethod)

    !tmp = self % gdomain % toString()
    !write(*,*) "current domain =", tmp % toString()

    !< calculate earch part
    enabled = self % gdomain % getEnabledGridMask()
    call self % findEnabledEqualSplitMethod % init(self % total, enabled)

    !< over each part
    !> 'extract' from given gridMask into new domain
    !> with the same as gridMask as well as those
    !> values marked in gridmask.
    do isplit = 1, self % findEnabledEqualSplitMethod % getSize()
      !write(*,*) "gridDomainPart...:: calculate:: --------------------------------------------------- "
      !write(*,*) "gridDomainPart...:: calculate:: isplit =", isplit
      !< get calculated grid mask
      tmpGMask = self % findEnabledEqualSplitMethod % get(isplit)
      !tmp = tmpGMask % toString()
      !write(*,*) "tmpGMask =", tmp % toString()

      !< find new grid shape
      tmpGMaskShrinked = tmpGMask % shrink()
      !< find new border
      tmpBorderMaskedShrinked = .not. tmpGMaskShrinked
      !tmp = tmpGMaskShrinked % toString()
      !write(*,*) "tmpGMaskShrinked =", tmp % toString()

      !< select enabled grid cell values from current domain
      filteredDomain = self % gdomain % filter(tmpGMask)
      !tmp = filteredDomain % toString()
      !write(*,*) "gridDomainPart...:: calculate:: filteredDomain =", tmp % toString()

      !< extract new grid shape
      shrinkedGrid = tmpGMaskShrinked % getGrid()
      !< find enabled grid cells values from domain
      allocate(selectedGridDomain, source = filteredDomain % select(shrinkedGrid))
      enabledGMask = selectedGridDomain % getEnabledGridMask()

      !< build
      call self % newDomains(isplit) % init(shrinkedGrid, enabledGMask, tmpBorderMaskedShrinked)

      !< release
      deallocate(selectedGridDomain)
      deallocate(tmpGMask)
    end do
  end subroutine calculate


  function get(self) result (newDomains)
    !< it returns partitioned domains
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(in) :: self
    class(shr_igridDomain), allocatable :: newDomains(:) !< output
    allocate(newDomains, source = self % newDomains)
  end function get

end module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod

