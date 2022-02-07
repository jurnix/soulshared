!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model 
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainPartitioningMethod_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Abstract class to handle a method to split a shr_gridDomain into
!> several parts
!> 
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethod_abs

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  use shr_gridDomain_mod, only: shr_iGridDomain

  implicit none

  public :: shr_gridDomainPartitioningMethod

  logical, parameter :: ISDEBUG = .false.


  type, abstract :: shr_gridDomainPartitioningMethod
  contains
    procedure(iface_calculate), deferred :: calculate
    procedure(iface_get), deferred :: get
  end type shr_gridDomainPartitioningMethod

  abstract interface

    subroutine iface_calculate(self, domain)
      import :: shr_gridDomainPartitioningMethod, shr_iGridDomain
      !< compute partitions
      class(shr_gridDomainPartitioningMethod), intent(inout) :: self
      class(shr_iGridDomain), intent(in) :: domain
    end subroutine iface_calculate

    function iface_get(self) result (newDomains)
      import :: shr_gridDomainPartitioningMethod, shr_igridDomain
      !< it returns partitioned domains
      class(shr_gridDomainPartitioningMethod), intent(in) :: self
      class(shr_igridDomain), allocatable :: newDomains(:) !< output
    end function iface_get
  end interface

contains

end module shr_gridDomainPartitioningMethod_abs

