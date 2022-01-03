!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainSquared_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!> shr_gridDomainSquared
!>
!> A grid domain squared has the property all border gridcells are enabled
!------------------------------------------------------------------------------
module shr_gridDomainSquared_mod

	use shr_gridDomain_mod, only: shr_gridDomain
	use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
	use shr_gridMask_mod, only: shr_gridMask

	implicit none

	type, extends(shr_gridDomain) :: shr_gridDomainSquared

	contains
		procedure :: gridDomainSquared_initialize
		generic :: init => gridDomainSquared_initialize
	end type shr_gridDomainSquared

contains

	subroutine gridDomainSquared_initialize(self, gridDescriptor, enabled)
		!< initialize gridDomainSquared
		class(shr_gridDomainSquared), intent(inout) :: self
		class(shr_iGgridDescriptor), intent(in) :: gridDescriptor
		type(shr_gridMask), intent(in) :: enabled

		type(shr_gridMask) :: maskBounds
		call maskBounds % init(gridDescriptor, default = .false.)
		call self % shr_gridDomain % init(gridDescriptor, enabled, maskBounds)
	end subroutine gridDomainSquared_initialize


end module shr_gridDomainSquared_mod