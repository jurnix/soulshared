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
!> Converts shr_gridDomain into (multiple) shr_gridDomainSquared(s)
!>
!------------------------------------------------------------------------------
module shr_gridDomainToSquaredConverter_mod

	use shr_error_mod, only: raiseError
	use shr_gridDomain_mod, only: shr_igridDomain, shr_gridDomain
	use shr_gridDomainSquared_mod, only: shr_gridDomainSquared
	use shr_gridMask_mod, only: shr_igridMask
	use shr_gGrid_mod, only: shr_igGrid


  implicit none


	type :: shr_gridDomainToSquaredConverter
    class(shr_iGridDomain), allocatable :: domains(:)
		type(shr_gridDomainSquared), allocatable :: squares(:)
	contains
		procedure :: init => gridDomainToSquaredConverter_initialize
		procedure, private :: convert
		procedure, private :: isDomainSquared
		procedure, private :: toSingleSquaredDomain !< single
		procedure, private :: toManySquaredDomains !< many
		procedure :: get => getSquaredDomains
  end type shr_gridDomainToSquaredConverter


contains

  subroutine gridDomainToSquaredConverter_initialize(self, domains)
	  !< initialize self
	  !< domain is divided into multiple squared domains
	  class(shr_gridDomainToSquaredConverter), intent(inout) :: self
	  class(shr_igridDomain), intent(in) :: domains(:)

		call self % convert(domains)
	end subroutine gridDomainToSquaredConverter_initialize


	subroutine convert(self, domains)
		!< convert each domain to squared domain
		class(shr_gridDomainToSquaredConverter), intent(inout) :: self
		class(shr_igridDomain), intent(in) :: domains(:)
		self % squares = self % toManySquaredDomains(domains)
	end subroutine convert


	logical function isDomainSquared(self, domain)
		!< true if all maskBorder gridcells are enabled
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		class(shr_igridDomain), intent(in) :: domain
		class(shr_igridMask), allocatable :: maskBounds
		maskBounds = domain % getBorderGridMask()
		isDomainSquared = maskBounds % any()
	end function isDomainSquared


	function toSingleSquaredDomain(self, domain) result (newDMSquared)
	  !< 'domain' is transformed into shr_gridDomainSquared
		!< 'domain' must be squared' otherwise an error is raised
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		class(shr_igridDomain), intent(in) :: domain
		type(shr_gridDomainSquared) :: newDMSquared !< output

		class(shr_igGrid), allocatable :: grid
		class(shr_igridMask), allocatable :: gMaskEnabled

		if (.not. self % isDomainSquared(domain)) then
			call raiseError(__FILE__, "toSingleSquaredDomain", &
						"Given domain must be squared. But It is not!")
		end if

		grid = domain % getGrid()
		gMaskEnabled = domain % getEnabledGridMask()
		call newDMSquared % init(grid, gMaskEnabled)
	end function toSingleSquaredDomain


	function toManySquaredDomains(self, domains) result (newDMSquared)
		!< convert multiple domains into gridDomainSquared
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		class(shr_igridDomain), intent(in) :: domains(:)
		type(shr_gridDomainSquared), allocatable :: newDMSquared(:) !< output
		integer :: idom, ndoms
		ndoms = size(domains)
		allocate(newDMSquared(ndoms))
		do idom = 1, ndoms
			newDMSquared(idom) = self % toSingleSquaredDomain(domains(idom))
		end do
	end function toManySquaredDomains


	function getSquaredDomains(self) result (sqDomains)
		!< it returns the squared domains
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		type(shr_gridDomainSquared), allocatable :: sqDomains(:)
		integer :: idom, ndoms
		!< deep copy
		ndoms = size(self % squares)
		allocate(sqDomains(ndoms))
		do idom = 1, ndoms
			allocate(sqDomains(idom), source = self % squares(idom))
		end do
	end function getSquaredDomains

end module shr_gridDomainToSquaredConverter_mod