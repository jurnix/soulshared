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
!>
!> 
!------------------------------------------------------------------------------
module shr_gridDomainPartitioningMethod_abs

  use SHR_error_mod, only: raiseError
  use SHR_precision_mod, only: sp

  implicit none

  public :: shr_gridDomainPartitioningMethod

  logical, parameter :: ISDEBUG = .false.


  type, abstract :: shr_gridDomainPartitioningMethod
  contains
    procedure(iface_calculate), deferred :: calculate
  end type shr_gridDomainPartitioningMethod

  abstract interface
    subroutine iface_calculate(self)
      import :: shr_gridDomainPartitioningMethod
      !< compute partitions
      class(shr_gridDomainPartitioningMethod), intent(inout) :: self
    end subroutine iface_calculate
  end interface

contains

end module shr_gridDomainPartitioningMethod_abs

