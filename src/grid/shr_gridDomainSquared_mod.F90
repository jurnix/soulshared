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
!> A grid domain squared has the property of having no borders
!> In consequence, all mask cells are available.
!------------------------------------------------------------------------------
module shr_gridDomainSquared_mod

	use shr_gridDomain_mod, only: shr_gridDomain
	use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
	use shr_gridMask_mod, only: shr_igridMask, shr_gridMask
	use shr_gGrid_mod, only: shr_igGrid

	implicit none

	type, extends(shr_gridDomain) :: shr_gridDomainSquared

	contains
		procedure :: gridDomainSquared_initialize
		generic :: init => gridDomainSquared_initialize

		procedure :: toGridDomain
	end type shr_gridDomainSquared

contains

	subroutine gridDomainSquared_initialize(self, grid, enabled)
		!< initialize gridDomainSquared
		class(shr_gridDomainSquared), intent(inout) :: self
		class(shr_igGrid), intent(in) :: grid
		class(shr_igridMask), intent(in) :: enabled

		class(shr_igridMask), allocatable :: maskBounds
		allocate(shr_gridMask :: maskBounds)
		call maskBounds % init(grid, default = .false.)
		call self % shr_gridDomain % init(grid, enabled, maskBounds)
	end subroutine gridDomainSquared_initialize


	type(shr_gridDomain) function toGridDomain(self) result (newGDomain)
		!< converts from squared into grid domain
		class(shr_gridDomainSquared), intent(in) :: self
		class(shr_igGrid), allocatable :: grid
		class(shr_igridMask), allocatable :: gEnabledMask
		class(shr_igridMask), allocatable :: gBorderMask

		grid = self % getGrid()
		gEnabledMask = self % getEnabledGridMask()
		gBorderMask = self % getBorderGridMask()

		call newGDomain % init(grid, gEnabledMask, gBorderMask)
	end function toGridDomain
end module shr_gridDomainSquared_mod