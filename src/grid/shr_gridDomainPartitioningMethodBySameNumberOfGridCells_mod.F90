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
    allocate(self % current, self % total)
    self % current = current
    self % total = total
  end subroutine init


  subroutine calculate(self, domain)
    !< compute partitions
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(inout) :: self
    class(shr_iGridDomain), intent(in) :: domain
    allocate(self % gdomain, source = domain)

    !nenabled = domain % countEnabledGridcells()

  end subroutine calculate


  function get(self) result (newDomains)
    !< it returns partitioned domains
    class(shr_gridDomainPartitioningMethodBySameNumberOfGridcells), intent(in) :: self
    class(shr_igridDomain), allocatable :: newDomains(:) !< output
    allocate(newDomains, source = self % newDomains)
  end function get

end module shr_gridDomainPartitioningMethodBySameNumberOfGridcells_mod

