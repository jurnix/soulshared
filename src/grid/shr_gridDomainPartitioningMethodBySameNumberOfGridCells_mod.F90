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
!> shr_gridDomain is partitioned into the same number of consecutive gridcells
!> x x x x (3)
!> x x x x --->  4x4 positions to divide into 3
!> x x x x
!> x x x x
!> 
!> 1st
!> x x x x
!> x b b b
!> 
!> 2nd
!> b x x x
!> x x b b
!> 
!> 3rd
!> 
!> b b x x
!> x x x x
!> 
!> ---------------------------------------------------------------------
!> 
!> x x x x (3)
!> x - x x --->  14 positions to divide into 3
!> x x - x
!> x x x x
!> 
!> 
!> gridDomain
!> 1st
!> 
!> 
!> x x x x   -> gridmask	(x x x x)
!> 
!> 2nd
!> 
!> x - x x   -> gridmask	(x - x x)
!> x x b b		(x x    )
!> 
!> 3rd
!> 
!> b b - x   -> gridmask	(    - x)
!> x x x x		(x x x x)
!>
!>
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod

  use SHR_error_mod, only: raiseError

  use shr_gridDomainPartitioningMethod_abs, only: shr_gridDomainPartitioningMethod

  use shr_gridMask_mod, only: shr_gridMask_cast, shr_IgridMask
  use shr_gridDomain_mod, only: shr_gridDomain, shr_iGridDomain


  implicit none

  public :: shr_gridDomainPartitioningMethodBySameNumberOfGridcells, &
              GRID_DOMAIN_PARTITION_EQUAL

  logical, parameter :: ISDEBUG = .false.

  character(*), parameter :: GRID_DOMAIN_PARTITION_EQUAL = "equal"


  type, extends(shr_gridDomainPartitioningMethod) :: shr_gridDomainPartitioningMethodBySameNumberOfGridcells
    class(shr_igridDomain), allocatable :: gdomain !< input
    class(shr_igridDomain), allocatable :: newDomains(:) !< output

    !< arguments
    integer, allocatable :: current, total
  contains
    procedure :: init
    procedure :: calculate
    procedure :: get
  end type shr_gridDomainPartitioningMethodBySameNumberOfGridcells

contains

  subroutine init(self, current, total)
    !< initialize with custom arguments
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self
    integer, intent(in) :: current, total

    allocate(self % current, self % total)
    self % current = current
    self % total = total
  end subroutine init


  subroutine calculate(self, domain)
    !< compute partitions
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self
    class(shr_iGridDomain), intent(in) :: domain
    class(shr_IgridMask), allocatable :: emask
    allocate(self % gdomain, source = domain)

    !emask = self % gdomain % getEnabledGridMask()
    !call gridMaskSplit % init(emask, method=equal)
    !call gridMaskSplit % calculate()

  end subroutine calculate


  function get(self) result (newDomains)
    !< it returns partitioned domains
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(in) :: self
    class(shr_igridDomain), allocatable :: newDomains(:) !< output
    allocate(newDomains, source = self % newDomains)
  end function get

end module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod

