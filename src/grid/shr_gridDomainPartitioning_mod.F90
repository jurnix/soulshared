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
!>
!> 
!------------------------------------------------------------------------------
module shr_gridDomainPartitioning_mod

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridDomain_mod, only: shr_gridDomain
  use shr_gridDomainPartitioningMethod_abs, only: shr_gridDomainPartitioningMethod
  use shr_strings_mod, only: string
  !use shr_gridMaskFindClustersIterator_mod, only: shr_gridMaskFindClustersIterator

  implicit none

  !public ::

  logical, parameter :: ISDEBUG = .false.


  type :: shr_gridDomainPartitioning
    class(shr_gridDomain), allocatable :: domains(:)
    class(shr_gridDomainPartitioningMethod), allocatable :: partitionMethod
  contains
    procedure :: init
    procedure :: getDomains
  end type shr_gridDomainPartitioning

contains

  subroutine init(self, gdomain, method)
    !< initializa partitioning
    class(shr_gridDomainPartitioning), intent(inout) :: self
    class(shr_gridDomain), intent(in) :: gdomain
    type(string), intent(in) :: method

    !< call self % partitionMethod % calculate()
  end subroutine init


  function getDomains(self) result (gdomains)
    !< get domains found after partition
    class(shr_gridDomainPartitioning), intent(in) :: self
    class(shr_gridDomain), allocatable :: gdomains(:) !< output

  end function getDomains

end module shr_gridDomainPartitioning_mod

