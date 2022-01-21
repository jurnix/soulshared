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
!> Example:
!>
!> (gridDomain) 			found								gridDomainSquared
!>
!> - x x -					-> 1st - x x -  ->  x x
!> x x - -					-> 2d  x x - -  ->  x x
!> - - - -     ->
!> x x x x					-> 3rd x x x x  ->  x x x x
!> x x x x 								 x x x x 			x x x x
!>
!> gridDomainSquare include the correct gridDescriptor.
!> They can be put into place in case those are combined.
!>
!> Such feature is useful when writing/reading netcdf data.
!> Due to parallelism the grid is divided for each proc.
!> This causes non regular grids. Consequently IO data must be written
!> into chunks.
!------------------------------------------------------------------------------
module shr_gridDomainToSquaredConverter_mod

	use shr_error_mod, only: raiseError
	use shr_gridDomain_mod, only: shr_gridDomain, shr_igridDomain
	use shr_gridDomainSquared_mod, only: shr_gridDomainSquared
	use shr_gridMask_mod, only: shr_igridMask, shr_gridMask_cast

	use shr_gridMaskFindClustersMethod_mod, only: shr_IGridMaskFindClustersMethod, shr_ObjTogridMaskClusters_cast
	use shr_gridMaskFindClustersIterator_mod, only: shr_gridMaskFindClustersIterator

	use shr_gGridDescriptor_mod, only: shr_iGGridDescriptor
	use shr_gGrid_mod, only: shr_gGrid

  implicit none

	type :: shr_gridDomainToSquaredConverter
    type(shr_gridDomain), allocatable :: domain
		type(shr_gridDomainSquared), allocatable :: squares(:)
	contains
		procedure :: init => gridDomainToSquaredConverter_initialize
		procedure, private :: isDomainSquared

		procedure, private :: toSquaredDomain !< single
		procedure, private :: toSquaredDomains !< many
		generic :: convert => toSquaredDomain, toSquaredDomains

		procedure :: get => getSquaredDomains
  end type shr_gridDomainToSquaredConverter

contains

  subroutine gridDomainToSquaredConverter_initialize(self, domain, clustersMethod)
	  !< initialize self
	  !< domain is divided into multiple squared domains
	  class(shr_gridDomainToSquaredConverter), intent(inout) :: self
	  type(shr_gridDomain), intent(in) :: domain
		class(shr_IGridMaskFindClustersMethod), intent(inout) :: clustersMethod

		class(shr_igridMask), allocatable :: maskBounds

	  allocate(self % domain, source = domain)

		!< partition grid mask into multiple squared grid domains
		maskBounds = self % domain % getBorderGridMask()
		call clustersMethod % init(maskBounds)

	  self % squares = self % toSquaredDomains(clustersMethod)
	end subroutine gridDomainToSquaredConverter_initialize


	logical function isDomainSquared(self, domain)
		!< true if all maskBorder gridcells are enabled
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		class(shr_igridDomain), intent(in) :: domain
		class(shr_igridMask), allocatable :: maskBounds
		maskBounds = self % domain % getBorderGridMask()
		isDomainSquared = maskBounds % any()
	end function isDomainSquared


	function toSquaredDomain(self, domain) result (newDMSquared)
	  !< 'domain' is transformed into shr_gridDomainSquared
		!< 'domain' must be squared'
		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		class(shr_igridDomain), intent(in) :: domain
		type(shr_gridDomainSquared) :: newDMSquared !< output

		class(shr_gGrid), allocatable :: grid
		class(shr_igridMask), allocatable :: gMaskEnabled

		if (.not. self % isDomainSquared(domain)) then
			call raiseError(__FILE__, "toSquaredDomain", &
						"Given domain must be squared. But It is not!")
		end if

		grid = domain % getGrid()
		gMaskEnabled = domain % getEnabledGridMask()
		call newDMSquared % init(grid, gMaskEnabled)
	end function toSquaredDomain


	function toSquaredDomains(self, clustersMethod) result (sqDomains)
		!< in case it is not squared:
		!< -it returns multiple domains with squared property
		!<  (the domain is partitioned into multiple squared grid domains)
		!< otherwise it returns the new gridDomainSquared

		class(shr_gridDomainToSquaredConverter), intent(in) :: self
		class(shr_IGridMaskFindClustersMethod), intent(in) :: clustersMethod
		type(shr_gridDomainSquared), allocatable :: sqDomains(:) !< output

		class(shr_igridDomain), allocatable :: subDomain
		type(shr_gridMaskFindClustersIterator) :: clustersIterator
		class(shr_igridMask), allocatable :: newGMaskClustered
		class(*) , allocatable :: obj

		integer :: ndomains, idomain
		class(shr_igridMask), allocatable :: maskBounds
		logical, allocatable :: mask2d(:,:)

		if (self % isDomainSquared(self % domain)) then
			allocate(sqDomains(1))
			sqDomains = self % toSquaredDomain(self % domain)
			return
		end if

		ndomains = clustersMethod % getSize()
		allocate(sqDomains(ndomains))

		call clustersIterator % init(clustersMethod)
		idomain = 1
		do while (clustersIterator % hasNext())
		  obj = clustersIterator % getNext()
			!< change type
			call shr_gridMask_cast(obj, newGMaskClustered)
			subDomain = self % domain % filter(newGMaskClustered)
			sqDomains(idomain) = self % toSquaredDomain(subdomain)

			idomain = idomain + 1
		enddo

	end function toSquaredDomains


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