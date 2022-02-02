!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainPartitioning_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> grid domain Partitioning by chosing a method
!> - It instantiates requested type and runs the partitioning
!> 
!------------------------------------------------------------------------------
module shr_gridDomainPartitioning_mod

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridDomain_mod, only: shr_igridDomain
  use shr_gridDomainPartitioningMethod_abs, only: shr_gridDomainPartitioningMethod
  use shr_gridDomainPartitioningMethodBySquares_mod, only: &
                        shr_gridDomainPartitioningMethodBySquares, &
                        GRID_DOMAIN_PARTITION_SQUARED
  use shr_strings_mod, only: string

  implicit none

  public :: shr_gridDomainPartitioning

  logical, parameter :: ISDEBUG = .false.



  type :: shr_gridDomainPartitioning
    class(shr_igridDomain), allocatable :: domains(:)
    class(shr_gridDomainPartitioningMethod), allocatable :: partitionMethod
  contains
    procedure :: init
    procedure :: getDomains
  end type shr_gridDomainPartitioning

contains

  subroutine init(self, gdomain, method)
    !< initializa partitioning
    class(shr_gridDomainPartitioning), intent(inout) :: self
    class(shr_igridDomain), intent(in) :: gdomain
    type(string), intent(in) :: method

    if (method == GRID_DOMAIN_PARTITION_SQUARED) then
      allocate(shr_gridDomainPartitioningMethodBySquares :: self % partitionMethod)
    else
      call raiseError(__FILE__, "init", &
          "Partitioning method not found")
    end if
    call self % partitionMethod % init(gdomain)
    call self % partitionMethod % calculate()
  end subroutine init


  function getDomains(self) result (gdomains)
    !< get domains found after partition
    class(shr_gridDomainPartitioning), intent(in) :: self
    class(shr_igridDomain), allocatable :: gdomains(:) !< output
    allocate(gdomains, source = self % partitionMethod % get())
  end function getDomains

end module shr_gridDomainPartitioning_mod

