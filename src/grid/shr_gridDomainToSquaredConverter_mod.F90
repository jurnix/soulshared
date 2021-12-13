!------------------------------------------------------------------------------
!    Pekin University - Sophie Land Surface Model
!------------------------------------------------------------------------------
! MODULE        : shr_gridDomainToSquaredConverter_mod
!
!> @author
!> Albert Jornet Puig
!
! DESCRIPTION:
!>
!> Converts shr_gridDomain into multiple shr_gridDomainSquared
!------------------------------------------------------------------------------
module shr_gridDomainToSquaredConverter_mod

	use shr_gridDomain_mod, only: shr_gridDomain
	use shr_gridDomainSquared_mod, only: shr_gridDomainSquared
	use shr_gridMask_mod, only: shr_gridMask

  implicit none

	type :: shr_gridDomainToSquaredConverter
    type(shr_gridDomain), allocatable :: domain
		type(shr_gridDomainSquared), allocatable :: squares(:)
	contains
		procedure :: init => gridDomainToSquaredConverter_initialize
		procedure, private :: isDomainSquared
		procedure :: toSquaredDomains
		procedure :: getSquaredDomains
  end type shr_gridDomainToSquaredConverter

contains

  subroutine gridDomainToSquaredConverter_initialize(self, domain)
	  !< initialize self
	  !< domain is divided into multiple squared domains
	  class(shr_gridDomainToSquaredConverter), intent(inout) :: self
	  type(shr_gridDomain), intent(in) :: domain
	  allocate(self % domain, source = domain)
	  self % squares = self % toSquaredDomains()
	end subroutine gridDomainToSquaredConverter_initialize


	logical function isDomainSquared(self, domain)
		!< true if all maskBorder gridcells are enabled
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		type(shr_gridDomain), intent(in) :: domain
		type(shr_gridMask) :: maskBounds
		maskBounds = self % domain % getMaskBounds()
		isDomainSquared = maskBounds % any()
	end function isDomainSquared


	function toSquaredDomains(self) result (sqDomains)
		!< in case it is not squared
		!< it returns multiple domains with squared propery
		!< otherwise it returns itself
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		type(shr_gridDomainSquared), allocatable :: sqDomains(:)

		if (self % isDomainSquared(self % domain)) then
			!allocate(sqDomains(1))
			!sqDomains(1) = self %
			return
		end if

		!< how to partition:
		!< header, body, footer
		!< todo
	end function toSquaredDomains


	function getSquaredDomains(self) result (sqDomains)
		!< it returns the squared domains found
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		type(shr_gridDomainSquared), allocatable :: sqDomains(:)
		!allocate(sqDomains, source = self % sqDomains)
	end function getSquaredDomains

end module shr_gridDomainToSquaredConverter_mod